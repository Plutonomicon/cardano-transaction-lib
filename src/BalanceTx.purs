module BalanceTx
  ( Actual(..)
  , AddTxCollateralsError(..)
  , BalanceNonAdaOutsError(..)
  , BalanceTxError(..)
  , BalanceTxInsError(..)
  , CannotMinusError(..)
  , EvalExUnitsAndMinFeeError(..)
  , Expected(..)
  , GetPublicKeyTransactionInputError(..)
  , GetWalletAddressError(..)
  , GetWalletCollateralError(..)
  , ImpossibleError(..)
  , ReturnAdaChangeError(..)
  , UnattachedTransaction
  , UtxosAtError(..)
  , balanceTx
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Redeemer(Redeemer)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Utxo
  , _body
  , _collateral
  , _fee
  , _inputs
  , _networkId
  , _outputs
  , _plutusData
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.Value
  ( filterNonAda
  , geq
  , getLovelace
  , lovelaceValueOf
  , isAdaOnly
  , isPos
  , isZero
  , minus
  , mkCoin
  , mkValue
  , numNonAdaAssets
  , numNonAdaCurrencySymbols
  , sumTokenNameLengths
  , valueToCoin
  , valueToCoin'
  , Value
  )
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Data.Array ((\\), modifyAt)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt, fromInt, quot)
import Data.Either (Either(Left, Right), hush, note)
import Data.Enum (fromEnum) as Enum
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens', _1)
import Data.Lens.Getter ((^.))
import Data.Lens.Index (ix) as Lens
import Data.Lens.Setter ((.~), set, (?~), (%~))
import Data.List ((:), List(Nil), partition)
import Data.Log.Tag (tag)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe, Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import ProtocolParametersAlonzo
  ( adaOnlyWords
  , coinSize
  , lovelacePerUTxOWord
  , pidSize
  , protocolParamUTxOCostPerWord
  , utxoEntrySizeWithoutVal
  )
import QueryM
  ( ClientError
  , QueryM
  , RdmrPtrExUnits(RdmrPtrExUnits)
  , calculateMinFee
  , getWalletAddress
  , getWalletCollateral
  , evalTxExecutionUnits
  )
import QueryM.Utxos (utxosAt)
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import TxOutput (utxoIndexToUtxo)
import ReindexRedeemers (ReindexErrors, reindexSpentScriptRedeemers')
import Types.Natural (toBigInt) as Natural
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction (DataHash, TransactionInput)
import Types.UnbalancedTransaction (UnbalancedTx(UnbalancedTx), _transaction)
import Wallet (Wallet(Gero, Nami))

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs

--------------------------------------------------------------------------------
-- Errors for Balancing functions
--------------------------------------------------------------------------------
-- These derivations may need tweaking when testing to make sure they are easy
-- to read, especially with generic show vs newtype show derivations.
data BalanceTxError
  = GetWalletAddressError' GetWalletAddressError
  | GetWalletCollateralError' GetWalletCollateralError
  | UtxosAtError' UtxosAtError
  | ReturnAdaChangeError' ReturnAdaChangeError
  | AddTxCollateralsError' AddTxCollateralsError
  | GetPublicKeyTransactionInputError' GetPublicKeyTransactionInputError
  | BalanceTxInsError' BalanceTxInsError
  | BalanceNonAdaOutsError' BalanceNonAdaOutsError
  | EvalExUnitsAndMinFeeError' EvalExUnitsAndMinFeeError

derive instance genericBalanceTxError :: Generic BalanceTxError _

instance showBalanceTxError :: Show BalanceTxError where
  show = genericShow

data GetWalletAddressError = CouldNotGetWalletAddress

derive instance genericGetWalletAddressError :: Generic GetWalletAddressError _

instance showGetWalletAddressError :: Show GetWalletAddressError where
  show = genericShow

data GetWalletCollateralError = CouldNotGetCollateral

derive instance genericGetWalletCollateralError ::
  Generic GetWalletCollateralError _

instance showGetWalletCollateralError :: Show GetWalletCollateralError where
  show = genericShow

data UtxosAtError = CouldNotGetUtxos

derive instance genericUtxosAtError :: Generic UtxosAtError _

instance showUtxosAtError :: Show UtxosAtError where
  show = genericShow

data EvalExUnitsAndMinFeeError
  = EvalExUnitsError ClientError
  | EvalMinFeeError ClientError
  | ReindexRedeemersError ReindexErrors

derive instance genericEvalExUnitsAndMinFeeError ::
  Generic EvalExUnitsAndMinFeeError _

instance showEvalExUnitsAndMinFeeError :: Show EvalExUnitsAndMinFeeError where
  show = genericShow

data ReturnAdaChangeError
  = ReturnAdaChangeError String
  | ReturnAdaChangeImpossibleError String ImpossibleError
  | ReturnAdaChangeCalculateMinFee EvalExUnitsAndMinFeeError

derive instance genericReturnAdaChangeError :: Generic ReturnAdaChangeError _

instance showReturnAdaChangeError :: Show ReturnAdaChangeError where
  show = genericShow

data AddTxCollateralsError
  = CollateralUtxosUnavailable
  | AddTxCollateralsError

derive instance genericAddTxCollateralsError :: Generic AddTxCollateralsError _

instance showAddTxCollateralsError :: Show AddTxCollateralsError where
  show = genericShow

data GetPublicKeyTransactionInputError = CannotConvertScriptOutputToTxInput

derive instance genericGetPublicKeyTransactionInputError ::
  Generic GetPublicKeyTransactionInputError _

instance showGetPublicKeyTransactionInputError ::
  Show GetPublicKeyTransactionInputError where
  show = genericShow

data BalanceTxInsError
  = InsufficientTxInputs Expected Actual
  | BalanceTxInsCannotMinus CannotMinusError

derive instance genericBalanceTxInsError :: Generic BalanceTxInsError _

instance showBalanceTxInsError :: Show BalanceTxInsError where
  show = genericShow

data CannotMinusError = CannotMinus Actual

derive instance genericCannotMinusError :: Generic CannotMinusError _

instance showCannotMinusError :: Show CannotMinusError where
  show = genericShow

data CollectTxInsError = CollectTxInsInsufficientTxInputs BalanceTxInsError

derive instance genericCollectTxInsError :: Generic CollectTxInsError _

instance showCollectTxInsError :: Show CollectTxInsError where
  show = genericShow

newtype Expected = Expected Value

derive instance genericExpected :: Generic Expected _
derive instance newtypeExpected :: Newtype Expected _

instance showExpected :: Show Expected where
  show = genericShow

newtype Actual = Actual Value

derive instance genericActual :: Generic Actual _
derive instance newtypeActual :: Newtype Actual _

instance showActual :: Show Actual where
  show = genericShow

data BalanceNonAdaOutsError
  = InputsCannotBalanceNonAdaTokens
  | BalanceNonAdaOutsCannotMinus CannotMinusError

derive instance genericBalanceNonAdaOutsError ::
  Generic BalanceNonAdaOutsError _

instance showBalanceNonAdaOutsError :: Show BalanceNonAdaOutsError where
  show = genericShow

-- | Represents that an error reason should be impossible
data ImpossibleError = Impossible

derive instance genericImpossibleError :: Generic ImpossibleError _

instance showImpossibleError :: Show ImpossibleError where
  show = genericShow

--------------------------------------------------------------------------------
-- Type aliases, temporary placeholder types
--------------------------------------------------------------------------------

-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

type UnattachedTransaction = Transaction /\ Array
  (Redeemer /\ Maybe TransactionInput)

--------------------------------------------------------------------------------
-- Evaluation of fees and execution units, Updating redeemers
--------------------------------------------------------------------------------

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnattachedUnbalancedTx` and
-- the minimum fee.
evalExUnitsAndMinFee'
  :: UnattachedUnbalancedTx
  -> QueryM
       (Either EvalExUnitsAndMinFeeError (UnattachedUnbalancedTx /\ BigInt))
evalExUnitsAndMinFee' unattachedTx =
  runExceptT do
    -- Reindex `Spent` script redeemers:
    unattachedReindexedTx <- ExceptT $ reindexRedeemers unattachedTx
      <#> lmap ReindexRedeemersError
    -- Reattach datums and redeemers before evaluating ex units:
    let attachedTx = reattachDatumsAndRedeemers unattachedReindexedTx
    -- Evaluate transaction ex units:
    rdmrPtrExUnitsList <- ExceptT $ evalTxExecutionUnits attachedTx
      <#> lmap EvalExUnitsError
    let
      -- Set execution units received from the server:
      unattachedTxWithExUnits =
        updateTxExecutionUnits unattachedReindexedTx rdmrPtrExUnitsList
      -- Reattach datums and redeemers before calculating fees:
      attachedTxWithExUnits =
        reattachDatumsAndRedeemers unattachedTxWithExUnits
    -- Calculate the minimum fee for a transaction:
    minFee <- ExceptT $ calculateMinFee attachedTxWithExUnits
      <#> bimap EvalMinFeeError unwrap
    pure $ unattachedTxWithExUnits /\ minFee

evalExUnitsAndMinFee
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError (UnattachedUnbalancedTx /\ BigInt))
evalExUnitsAndMinFee =
  map (lmap EvalExUnitsAndMinFeeError') <<< evalExUnitsAndMinFee'

reindexRedeemers
  :: UnattachedUnbalancedTx
  -> QueryM (Either ReindexErrors UnattachedUnbalancedTx)
reindexRedeemers
  unattachedTx@(UnattachedUnbalancedTx { redeemersTxIns }) =
  let
    inputs = unattachedTx ^. _body' <<< _inputs
  in
    reindexSpentScriptRedeemers' inputs redeemersTxIns <#>
      map \redeemersTxInsReindexed ->
        unattachedTx # _redeemersTxIns .~ redeemersTxInsReindexed

-- | Reattaches datums and redeemers to the transaction.
reattachDatumsAndRedeemers :: UnattachedUnbalancedTx -> Transaction
reattachDatumsAndRedeemers
  (UnattachedUnbalancedTx { unbalancedTx, datums, redeemersTxIns }) =
  let
    transaction = unbalancedTx ^. _transaction
  in
    transaction # _witnessSet <<< _plutusData ?~ map unwrap datums
      # _witnessSet <<< _redeemers ?~ map fst redeemersTxIns

updateTxExecutionUnits
  :: UnattachedUnbalancedTx -> Array RdmrPtrExUnits -> UnattachedUnbalancedTx
updateTxExecutionUnits unattachedTx rdmrPtrExUnits =
  unattachedTx #
    _redeemersTxIns %~ flip setRdmrsExecutionUnits rdmrPtrExUnits

setRdmrsExecutionUnits
  :: Array (Redeemer /\ Maybe TransactionInput)
  -> Array RdmrPtrExUnits
  -> Array (Redeemer /\ Maybe TransactionInput)
setRdmrsExecutionUnits rs xxs =
  case Array.uncons xxs of
    Nothing -> rs
    Just { head: RdmrPtrExUnits x, tail: xs } ->
      let
        ixMaybe = flip Array.findIndex rs $ \(Redeemer rdmr /\ _) ->
          Enum.fromEnum rdmr.tag == x.rdmrPtrTag
            && rdmr.index == Natural.toBigInt x.rdmrPtrIdx
      in
        ixMaybe # maybe (setRdmrsExecutionUnits rs xs) \ix ->
          flip setRdmrsExecutionUnits xs $
            rs # Lens.ix ix %~ \(Redeemer rec /\ txOutRef) ->
              let
                mem = Natural.toBigInt x.exUnitsMem
                steps = Natural.toBigInt x.exUnitsSteps
              in
                Redeemer rec { exUnits = { mem, steps } } /\ txOutRef

--------------------------------------------------------------------------------
-- `UnattachedUnbalancedTx` Lenses
--------------------------------------------------------------------------------

_unbalancedTx :: Lens' UnattachedUnbalancedTx UnbalancedTx
_unbalancedTx = lens' \(UnattachedUnbalancedTx rec@{ unbalancedTx }) ->
  unbalancedTx /\
    \ubTx -> UnattachedUnbalancedTx rec { unbalancedTx = ubTx }

_transaction' :: Lens' UnattachedUnbalancedTx Transaction
_transaction' = lens' \unattachedTx ->
  unattachedTx ^. _unbalancedTx <<< _transaction /\
    \tx -> unattachedTx # _unbalancedTx %~ (_transaction .~ tx)

_body' :: Lens' UnattachedUnbalancedTx TxBody
_body' = lens' \unattachedTx ->
  unattachedTx ^. _transaction' <<< _body /\
    \txBody -> unattachedTx # _transaction' %~ (_body .~ txBody)

_redeemersTxIns
  :: Lens' UnattachedUnbalancedTx (Array (Redeemer /\ Maybe TransactionInput))
_redeemersTxIns = lens' \(UnattachedUnbalancedTx rec@{ redeemersTxIns }) ->
  redeemersTxIns /\
    \rdmrs -> UnattachedUnbalancedTx rec { redeemersTxIns = rdmrs }

--------------------------------------------------------------------------------
-- Balancing functions and helpers
--------------------------------------------------------------------------------
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L54
-- FIX ME: UnbalancedTx contains requiredSignatories which would be a part of
-- multisig but we don't have such functionality ATM.

-- | Balances an unbalanced transaction. For submitting a tx via Nami, the
-- | utxo set shouldn't include the collateral which is vital for balancing.
-- | In particular, the transaction inputs must not include the collateral.
balanceTx
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError UnattachedTransaction)
balanceTx unattachedTx@(UnattachedUnbalancedTx { unbalancedTx: t }) = do
  let (UnbalancedTx { transaction: unbalancedTx, utxoIndex }) = t
  networkId <- (unbalancedTx ^. _body <<< _networkId) #
    maybe (asks _.networkId) pure
  let unbalancedTx' = unbalancedTx # _body <<< _networkId ?~ networkId
  runExceptT do
    -- Get own wallet address, collateral and utxo set:
    ownAddr <- ExceptT $ getWalletAddress <#>
      note (GetWalletAddressError' CouldNotGetWalletAddress)
    utxos <- ExceptT $ utxosAt ownAddr <#>
      (note (UtxosAtError' CouldNotGetUtxos) >>> map unwrap)
    collateral <- ExceptT $ getWalletCollateral <#>
      note (GetWalletCollateralError' CouldNotGetCollateral)
    wallet <- asks _.wallet
    let
      -- Combines utxos at the user address and those from any scripts
      -- involved with the contract in the unbalanced transaction.
      allUtxos :: Utxo
      allUtxos = utxos `Map.union` utxoIndexToUtxo networkId utxoIndex

      -- After adding collateral, we need to balance the inputs and
      -- non-Ada outputs before looping, i.e. we need to add input fees
      -- for the Ada only collateral. No MinUtxos required. Perhaps
      -- for some wallets this step can be skipped and we can go straight
      -- to prebalancer.
      unbalancedCollTx = case wallet of
        Just (Nami _) -> addTxCollateral unbalancedTx' collateral
        Just (Gero _) -> addTxCollateral unbalancedTx' collateral
        _ -> unbalancedTx'

    -- Logging Unbalanced Tx with collateral added:
    logTx "Unbalanced Collaterised Tx " allUtxos unbalancedCollTx
    -- Prebalance collaterised tx without fees:
    ubcTx <- except $
      prebalanceCollateral zero allUtxos ownAddr unbalancedCollTx
    -- Prebalance collaterised tx with fees:
    let unattachedTx' = unattachedTx # _transaction' .~ ubcTx
    _ /\ fees <- ExceptT $ evalExUnitsAndMinFee unattachedTx'
    ubcTx' <- except $
      prebalanceCollateral (fees + feeBuffer) allUtxos ownAddr ubcTx
    -- Loop to balance non-Ada assets
    nonAdaBalancedCollTx <- ExceptT $ loop allUtxos ownAddr [] $ unattachedTx' #
      _transaction' .~ ubcTx'
    -- Return excess Ada change to wallet:
    unsignedTx <- ExceptT $
      returnAdaChangeAndFinalizeFees ownAddr allUtxos nonAdaBalancedCollTx <#>
        lmap ReturnAdaChangeError'
    let
      unattachedTx'' = unsignedTx ^. _transaction'
        /\ unsignedTx ^. _redeemersTxIns
      -- Sort inputs at the very end so it behaves as a Set.
      sortedUnsignedTx = fst unattachedTx'' # _body <<< _inputs %~ Array.sort
    -- Logs final balanced tx and returns it
    logTx "Post-balancing Tx " allUtxos sortedUnsignedTx
    except $ Right (unattachedTx'' # _1 .~ sortedUnsignedTx)
  where
  prebalanceCollateral
    :: BigInt
    -> Utxo
    -> Address
    -> Transaction
    -> Either BalanceTxError Transaction
  prebalanceCollateral fees utxos ownAddr tx =
    balanceTxIns utxos fees (tx ^. _body)
      >>= balanceNonAdaOuts ownAddr utxos
      <#> flip (set _body) tx

  loop
    :: Utxo
    -> Address
    -> MinUtxos
    -> UnattachedUnbalancedTx
    -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
  loop utxoIndex' ownAddr' prevMinUtxos' unattachedTx' = do
    let
      Transaction { body: txBody'@(TxBody txB) } =
        unattachedTx' ^. _transaction'

      nextMinUtxos' :: MinUtxos
      nextMinUtxos' =
        calculateMinUtxos $ txB.outputs \\ map fst prevMinUtxos'

      minUtxos' :: MinUtxos
      minUtxos' = prevMinUtxos' <> nextMinUtxos'

    unattachedTxWithBalancedBody <- chainedBalancer minUtxos' utxoIndex'
      ownAddr'
      unattachedTx'

    case unattachedTxWithBalancedBody of
      Left err -> pure $ Left err
      Right unattachedTxWithBalancedBody' ->
        let
          balancedTxBody = unattachedTxWithBalancedBody' ^. _body'
        in
          if txBody' == balancedTxBody then
            pure $ Right unattachedTxWithBalancedBody'
          else
            loop utxoIndex' ownAddr' minUtxos' unattachedTxWithBalancedBody'

  chainedBalancer
    :: MinUtxos
    -> Utxo
    -> Address
    -> UnattachedUnbalancedTx
    -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
  chainedBalancer minUtxos' utxoIndex' ownAddr' unattachedTx' =
    runExceptT do
      let Transaction tx@{ body: txBody' } = unattachedTx' ^. _transaction'
      txBodyWithoutFees' <- except $
        preBalanceTxBody minUtxos' zero utxoIndex' ownAddr' txBody'
      let
        tx' = wrap tx { body = txBodyWithoutFees' }
        unattachedTx'' = unattachedTx' # _unbalancedTx <<< _transaction .~ tx'
      unattachedTx''' /\ fees' <- ExceptT $
        evalExUnitsAndMinFee unattachedTx''

      except <<< map (\body -> unattachedTx''' # _body' .~ body) $
        preBalanceTxBody minUtxos' (fees' + feeBuffer) utxoIndex' ownAddr'
          txBody'

  -- We expect the user has a minimum amount of Ada (this buffer) on top of
  -- their transaction, so by the time the prebalancer finishes, should it
  -- succeed, there will be some excess Ada (since all non Ada is balanced).
  -- In particular, this excess Ada will be at least this buffer. This is
  -- important for when returnAdaChange is called to return Ada change back
  -- to the user. The idea is this buffer provides enough so that returning
  -- excess Ada is covered by two main scenarios (see returnAdaChange for more
  -- details):
  -- 1) A new utxo doesn't need to be created (no extra fees).
  -- 2) A new utxo needs to be created but the buffer provides enough Ada to
  -- create the utxo, cover any extra fees without a further need for looping.
  -- This buffer could potentially be calculated exactly using (the Haskell server)
  -- https://github.com/input-output-hk/plutus/blob/8abffd78abd48094cfc72f1ad7b81b61e760c4a0/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek/Internal.hs#L723
  -- The trade off for this set up is any transaction requires this buffer on
  -- top of their transaction as input.
  feeBuffer :: BigInt
  feeBuffer = fromInt 500000

-- CIP-30 wallets provide a 5 Ada collateral that we should add the tx before
-- balancing.
addTxCollateral :: Transaction -> TransactionUnspentOutput -> Transaction
addTxCollateral transaction (TransactionUnspentOutput { input }) =
  transaction # _body <<< _collateral ?~
    Array.singleton input

-- Logging for Transaction type without returning Transaction
logTx
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadLogger m
  => String
  -> Utxo
  -> Transaction
  -> m Unit
logTx msg utxos (Transaction { body: body'@(TxBody body) }) =
  traverse_ (Logger.trace (tag msg mempty))
    [ "Input Value: " <> show (getInputValue utxos body')
    , "Output Value: " <> show (Array.foldMap getAmount body.outputs)
    , "Fees: " <> show body.fee
    ]

-- Transaction should be pre-balanced at this point, and the Ada value of the
-- inputs should be greater than or equal to the value of the outputs.
-- This should be called with a Tx with min Ada in each output utxo,
-- namely, after "loop".
returnAdaChangeAndFinalizeFees
  :: Address
  -> Utxo
  -> UnattachedUnbalancedTx
  -> QueryM (Either ReturnAdaChangeError UnattachedUnbalancedTx)
returnAdaChangeAndFinalizeFees changeAddr utxos unattachedTx =
  runExceptT do
    -- Calculate min fee before returning ada change to the owner's address:
    unattachedTxAndFees@(_ /\ fees) <-
      ExceptT $ evalExUnitsAndMinFee' unattachedTx
        <#> lmap ReturnAdaChangeCalculateMinFee
    -- If required, create an extra output to return the change:
    unattachedTxWithChangeTxOut /\ { recalculateFees } <-
      except $ returnAdaChange changeAddr utxos unattachedTxAndFees
    case recalculateFees of
      false -> except <<< Right $
        -- Set min fee and return tx without recalculating fees:
        unattachedTxSetFees unattachedTxWithChangeTxOut fees
      true -> do
        -- Recalculate min fee, then adjust the change output:
        unattachedTx' /\ fees' <-
          ExceptT $ evalExUnitsAndMinFee' unattachedTxWithChangeTxOut
            <#> lmap ReturnAdaChangeCalculateMinFee
        except $
          adjustAdaChangeAndSetFees unattachedTx' fees' (fees' - fees)
  where
  adjustAdaChangeAndSetFees
    :: UnattachedUnbalancedTx
    -> BigInt
    -> BigInt
    -> Either ReturnAdaChangeError UnattachedUnbalancedTx
  adjustAdaChangeAndSetFees unattachedTx' fees feesDelta
    | feesDelta <= zero = Right $
        unattachedTxSetFees unattachedTx' fees
    | otherwise =
        let
          txOutputs :: Array TransactionOutput
          txOutputs = unattachedTx' ^. _body' <<< _outputs

          returnAda :: BigInt
          returnAda = fromMaybe zero $
            Array.head txOutputs <#> \(TransactionOutput rec) ->
              (valueToCoin' rec.amount) - feesDelta

          utxoCost :: BigInt
          utxoCost = getLovelace protocolParamUTxOCostPerWord

          changeMinUtxo :: BigInt
          changeMinUtxo = adaOnlyWords * utxoCost
        in
          case returnAda >= changeMinUtxo of
            true -> do
              newOutputs <- updateChangeTxOutputValue returnAda txOutputs
              pure $
                unattachedTx' # _body' %~ \(TxBody txBody) ->
                  wrap txBody { outputs = newOutputs, fee = wrap fees }
            false ->
              Left $
                ReturnAdaChangeError
                  "returnAda does not cover min utxo requirement for \
                  \single Ada-only output."

  unattachedTxSetFees
    :: UnattachedUnbalancedTx -> BigInt -> UnattachedUnbalancedTx
  unattachedTxSetFees unattachedTx' fees =
    unattachedTx' #
      _body' <<< _fee .~ wrap fees

  updateChangeTxOutputValue
    :: BigInt
    -> Array TransactionOutput
    -> Either ReturnAdaChangeError (Array TransactionOutput)
  updateChangeTxOutputValue returnAda =
    note (ReturnAdaChangeError "Couldn't modify utxo to return change.")
      <<< modifyAt zero
        \(TransactionOutput rec) -> TransactionOutput
          rec { amount = lovelaceValueOf returnAda }

returnAdaChange
  :: Address
  -> Utxo
  -> UnattachedUnbalancedTx /\ BigInt
  -> Either ReturnAdaChangeError
       (UnattachedUnbalancedTx /\ { recalculateFees :: Boolean })
returnAdaChange changeAddr utxos (unattachedTx /\ fees) =
  let
    TxBody txBody = unattachedTx ^. _body'

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    inputValue :: Value
    inputValue = getInputValue utxos (wrap txBody)

    inputAda :: BigInt
    inputAda = getLovelace $ valueToCoin inputValue

    outputValue :: Value
    outputValue = Array.foldMap getAmount txOutputs

    outputAda :: BigInt
    outputAda = getLovelace $ valueToCoin outputValue

    returnAda :: BigInt
    returnAda = inputAda - outputAda - fees
  in
    case compare returnAda zero of
      EQ ->
        Right $
          unattachedTx /\ { recalculateFees: false }
      LT ->
        Left $
          ReturnAdaChangeImpossibleError
            "Not enough Input Ada to cover output and fees after prebalance."
            Impossible
      GT ->
        let
          changeTxOutput :: TransactionOutput
          changeTxOutput = wrap
            { address: changeAddr
            , amount: lovelaceValueOf returnAda
            , dataHash: Nothing
            }

          unattachedTxWithChangeTxOut :: UnattachedUnbalancedTx
          unattachedTxWithChangeTxOut =
            unattachedTx # _body' <<< _outputs %~
              Array.cons changeTxOutput
        in
          Right $
            unattachedTxWithChangeTxOut /\ { recalculateFees: true }

calculateMinUtxos :: Array TransactionOutput -> MinUtxos
calculateMinUtxos = map (\a -> a /\ calculateMinUtxo a)

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
-- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
-- | Given an array of transaction outputs, return the paired amount of lovelaces
-- | required by each utxo.
calculateMinUtxo :: TransactionOutput -> BigInt
calculateMinUtxo txOut = unwrap lovelacePerUTxOWord * utxoEntrySize txOut
  where
  -- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
  -- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
  utxoEntrySize :: TransactionOutput -> BigInt
  utxoEntrySize (TransactionOutput txOut') =
    let
      outputValue :: Value
      outputValue = txOut'.amount
    in
      if isAdaOnly outputValue then utxoEntrySizeWithoutVal + coinSize -- 29 in Alonzo
      else utxoEntrySizeWithoutVal
        + size outputValue
        + dataHashSize txOut'.dataHash

-- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
-- | Calculates how many words are needed depending on whether the datum is
-- | hashed or not. 10 words for a hashed datum and 0 for no hash. The argument
-- | to the function is the datum hash found in `TransactionOutput`.
dataHashSize :: Maybe DataHash -> BigInt -- Should we add type safety?
dataHashSize Nothing = zero
dataHashSize (Just _) = fromInt 10

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
-- See "size"
size :: Value -> BigInt
size v = fromInt 6 + roundupBytesToWords b
  where
  b :: BigInt
  b = numNonAdaAssets v * fromInt 12
    + sumTokenNameLengths v
    + numNonAdaCurrencySymbols v * pidSize

  -- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
  -- Converts bytes to 8-byte long words, rounding up
  roundupBytesToWords :: BigInt -> BigInt
  roundupBytesToWords b' = quot (b' + (fromInt 7)) $ fromInt 8

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L116
preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxo
  -> Address
  -> TxBody
  -> Either BalanceTxError TxBody
preBalanceTxBody minUtxos fees utxos ownAddr txBody =
  -- -- Take a single Ada only utxo collateral
  -- addTxCollaterals utxos txBody
  --   >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
  --   >>= balanceNonAdaOuts ownAddr utxos
  addLovelaces minUtxos txBody # pure
    >>= balanceTxIns utxos fees -- Adding more inputs if required
    >>= balanceNonAdaOuts ownAddr utxos

-- addTxCollaterals :: Utxo -> TxBody -> Either BalanceTxError TxBody
-- addTxCollaterals utxo txBody =
--   addTxCollaterals' utxo txBody # lmap AddTxCollateralsError

-- -- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L211
-- -- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- -- | (suboptimally we just pick a random utxo from the tx inputs). TO DO: upgrade
-- -- | to a better coin selection algorithm.
-- addTxCollaterals' :: Utxo -> TxBody -> Either AddTxCollateralsError TxBody
-- addTxCollaterals' utxos (TxBody txBody) = do
--   let
--     txIns :: Array TransactionInput
--     txIns = utxosToTransactionInput $ filterAdaOnly utxos
--   txIn :: TransactionInput <- findPubKeyTxIn txIns
--   pure $ wrap txBody { collateral = Just (Array.singleton txIn) }
--   where
--   filterAdaOnly :: Utxo -> Utxo
--   filterAdaOnly = Map.filter (isAdaOnly <<< getAmount)

--   -- Take head for collateral - can use better coin selection here.
--   findPubKeyTxIn
--     :: Array TransactionInput
--     -> Either AddTxCollateralsError TransactionInput
--   findPubKeyTxIn =
--     note CollateralUtxosUnavailable <<< Array.head

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Get `TransactionInput` such that it is associated to `PaymentCredentialKey`
-- | and not `PaymentCredentialScript`, i.e. we want wallets only
getPublicKeyTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either GetPublicKeyTransactionInputError TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  note CannotConvertScriptOutputToTxInput $ do
    paymentCred <- unwrap txOut # (_.address >>> addressPaymentCred)
    -- TEST ME: using StakeCredential to determine whether wallet or script
    paymentCred # withStakeCredential
      { onKeyHash: const $ pure txOutRef
      , onScriptHash: const Nothing
      }

--------------------------------------------------------------------------------
-- Balance transaction inputs
--------------------------------------------------------------------------------

balanceTxIns :: Utxo -> BigInt -> TxBody -> Either BalanceTxError TxBody
balanceTxIns utxos fees txbody =
  balanceTxIns' utxos fees txbody # lmap BalanceTxInsError'

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- Notice we aren't using protocol parameters for utxo cost per word.
balanceTxIns' :: Utxo -> BigInt -> TxBody -> Either BalanceTxInsError TxBody
balanceTxIns' utxos fees (TxBody txBody) = do
  let
    utxoCost :: BigInt
    utxoCost = getLovelace protocolParamUTxOCostPerWord

    changeMinUtxo :: BigInt
    changeMinUtxo = adaOnlyWords * utxoCost

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    mintVal :: Value
    mintVal = maybe mempty (mkValue (mkCoin zero) <<< unwrap) txBody.mint

  nonMintedValue <- note (BalanceTxInsCannotMinus $ CannotMinus $ wrap mintVal)
    $ Array.foldMap getAmount txOutputs `minus` mintVal

  -- Useful spies for debugging:
  -- let x = spy "nonMintedVal" nonMintedValue
  --     y = spy "feees" fees
  --     z = spy "changeMinUtxo" changeMinUtxo
  --     a = spy "txBody" txBody

  let
    minSpending :: Value
    minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  -- a = spy "minSpending" minSpending

  txIns :: Array TransactionInput <-
    lmap
      ( \(CollectTxInsInsufficientTxInputs insufficientTxInputs) ->
          insufficientTxInputs
      )
      $ collectTxIns txBody.inputs utxos minSpending
  -- Original code uses Set append which is union. Array unions behave
  -- a little differently as it removes duplicates in the second argument.
  -- but all inputs should be unique anyway so I think this is fine.
  -- Note, this does not sort automatically unlike Data.Set
  pure $ wrap
    txBody
      { inputs = Array.union txIns txBody.inputs
      }

--https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Getting the necessary input utxos to cover the fees for the transaction
collectTxIns
  :: Array TransactionInput
  -> Utxo
  -> Value
  -> Either CollectTxInsError (Array TransactionInput)
collectTxIns originalTxIns utxos value =
  if isSufficient updatedInputs then pure updatedInputs
  else
    Left $ CollectTxInsInsufficientTxInputs $
      InsufficientTxInputs (Expected value)
        (Actual $ txInsValue utxos updatedInputs)
  where
  updatedInputs :: Array TransactionInput
  updatedInputs =
    Foldable.foldl
      ( \newTxIns txIn ->
          if Array.elem txIn newTxIns || isSufficient newTxIns then newTxIns
          else Array.insert txIn newTxIns -- treat as a set.
      )
      originalTxIns
      $ utxosToTransactionInput utxos

  -- Useful spies for debugging:
  -- x = spy "collectTxIns:value" value
  -- y = spy "collectTxIns:txInsValueOG" (txInsValue utxos originalTxIns)
  -- z = spy "collectTxIns:txInsValueNEW" (txInsValue utxos updatedInputs)

  isSufficient :: Array TransactionInput -> Boolean
  isSufficient txIns' =
    not (Array.null txIns') && (txInsValue utxos txIns') `geq` value

txInsValue :: Utxo -> Array TransactionInput -> Value
txInsValue utxos =
  Array.foldMap getAmount <<< Array.mapMaybe (flip Map.lookup utxos)

utxosToTransactionInput :: Utxo -> Array TransactionInput
utxosToTransactionInput =
  Array.mapMaybe (hush <<< getPublicKeyTransactionInput) <<< Map.toUnfoldable

balanceNonAdaOuts
  :: Address
  -> Utxo
  -> TxBody
  -> Either BalanceTxError TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  balanceNonAdaOuts' changeAddr utxos txBody # lmap BalanceNonAdaOutsError'

-- FIX ME: (payment credential) address for change substitute for pkh (Address)
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L225
-- | We need to balance non ada values as part of the prebalancer before returning
-- | excess Ada to the owner.
balanceNonAdaOuts'
  :: Address
  -> Utxo
  -> TxBody
  -> Either BalanceNonAdaOutsError TxBody
balanceNonAdaOuts' changeAddr utxos txBody'@(TxBody txBody) = do
  let -- FIX ME: Similar to Address issue, need pkh.
    -- payCredentials :: PaymentCredential
    -- payCredentials = addressPaymentCredentials changeAddr

    -- FIX ME: once both BaseAddresses are merged into one.
    -- pkh :: PubKeyHash
    -- pkh = addressPubKeyHash (unwrap changeAddr)."AddrType"

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    inputValue :: Value
    inputValue = getInputValue utxos txBody'

    outputValue :: Value
    outputValue = Array.foldMap getAmount txOutputs

    mintVal :: Value
    mintVal = maybe mempty (mkValue (mkCoin zero) <<< unwrap) txBody.mint

  nonMintedOutputValue <-
    note (BalanceNonAdaOutsCannotMinus $ CannotMinus $ wrap mintVal)
      $ outputValue `minus` mintVal

  let (nonMintedAdaOutputValue :: Value) = filterNonAda nonMintedOutputValue

  nonAdaChange <-
    note
      ( BalanceNonAdaOutsCannotMinus $ CannotMinus $ wrap
          nonMintedAdaOutputValue
      )
      $ filterNonAda inputValue `minus` nonMintedAdaOutputValue

  let
    -- Useful spies for debugging:
    -- a = spy "balanceNonAdaOuts'nonMintedOutputValue" nonMintedOutputValue
    -- b = spy "balanceNonAdaOuts'nonMintedAdaOutputValue" nonMintedAdaOutputValue
    -- c = spy "balanceNonAdaOuts'nonAdaChange" nonAdaChange
    -- d = spy "balanceNonAdaOuts'inputValue" inputValue

    outputs :: Array TransactionOutput
    outputs =
      Array.fromFoldable $
        case
          partition
            ((==) changeAddr <<< _.address <<< unwrap) --  <<< txOutPaymentCredentials)

            $ Array.toUnfoldable txOutputs
          of
          { no: txOuts, yes: Nil } ->
            TransactionOutput
              { address: changeAddr
              , amount: nonAdaChange
              , dataHash: Nothing
              } : txOuts
          { no: txOuts'
          , yes: TransactionOutput txOut@{ amount: v } : txOuts
          } ->
            TransactionOutput
              txOut { amount = v <> nonAdaChange } : txOuts <> txOuts'

  -- Original code uses "isNat" because there is a guard against zero, see
  -- isPos for more detail.
  if isPos nonAdaChange then pure $ wrap txBody { outputs = outputs }
  else if isZero nonAdaChange then pure $ wrap txBody
  else Left InputsCannotBalanceNonAdaTokens

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

-- | Add min lovelaces to each tx output
addLovelaces :: MinUtxos -> TxBody -> TxBody
addLovelaces minLovelaces (TxBody txBody) =
  let
    lovelacesAdded :: Array TransactionOutput
    lovelacesAdded =
      map
        ( \txOut' ->
            let
              txOut = unwrap txOut'

              outValue :: Value
              outValue = txOut.amount

              lovelaces :: BigInt
              lovelaces = getLovelace $ valueToCoin outValue

              minUtxo :: BigInt
              minUtxo = fromMaybe zero $ Foldable.lookup txOut' minLovelaces
            in
              wrap
                txOut
                  { amount =
                      outValue
                        <> lovelaceValueOf (max zero $ minUtxo - lovelaces)
                  }
        )
        txBody.outputs
  in
    wrap txBody { outputs = lovelacesAdded }

getInputValue :: Utxo -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)
