module BalanceTx
  ( module BalanceTxErrorExport
  , balanceTx
  , balanceTxWithAddress
  , FinalizedTransaction(FinalizedTransaction)
  ) where

import Prelude

import BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddress
      , ExUnitsEvaluationFailed
      , InsufficientTxInputs
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  )
import BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError(..)
  , Expected(Expected)
  , printTxEvaluationFailure
  ) as BalanceTxErrorExport
import BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types.Transaction
  ( Redeemer(Redeemer)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Utxos
  , _body
  , _collateral
  , _fee
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _plutusData
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.Value
  ( Coin(Coin)
  , Value
  , geq
  , getNonAdaAsset
  , minus
  , mkValue
  , valueToCoin'
  )
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable (foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens')
import Data.Lens.Getter ((^.))
import Data.Lens.Index (ix) as Lens
import Data.Lens.Setter ((.~), (?~), (%~))
import Data.Log.Tag (tag)
import Data.Map (fromFoldable, lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import QueryM (QueryM, QueryMExtended)
import QueryM (evaluateTxOgmios, getWalletAddress) as QueryM
import QueryM.MinFee (calculateMinFee) as QueryM
import QueryM.Ogmios (TxEvaluationResult(TxEvaluationResult)) as Ogmios
import QueryM.Utxos (utxosAt, filterLockedUtxos, getWalletCollateral)
import ReindexRedeemers (ReindexErrors, reindexSpentScriptRedeemers')
import Serialization (convertTransaction, toBytes) as Serialization
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import Transaction (setScriptDataHash)
import Types.Natural (toBigInt) as Natural
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (UnbalancedTx, _transaction, _utxoIndex)
import Untagged.Union (asOneOf)

type BalanceTxM (a :: Type) = ExceptT BalanceTxError (QueryMExtended ()) a

--------------------------------------------------------------------------------
-- Evaluation of fees and execution units, Updating redeemers
--------------------------------------------------------------------------------

evalTxExecutionUnits
  :: Transaction
  -> UnattachedUnbalancedTx
  -> BalanceTxM Ogmios.TxEvaluationResult
evalTxExecutionUnits tx unattachedTx = do
  txBytes <- liftEffect
    ( wrap <<< Serialization.toBytes <<< asOneOf <$>
        Serialization.convertTransaction tx
    )
  ExceptT $ QueryM.evaluateTxOgmios txBytes
    <#> lmap (ExUnitsEvaluationFailed unattachedTx) <<< unwrap

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnattachedUnbalancedTx` and
-- the minimum fee.
evalExUnitsAndMinFee
  :: UnattachedUnbalancedTx -> BalanceTxM (UnattachedUnbalancedTx /\ BigInt)
evalExUnitsAndMinFee unattachedTx = do
  -- Reindex `Spent` script redeemers:
  reindexedUnattachedTx <-
    ExceptT $ reindexRedeemers unattachedTx <#> lmap ReindexRedeemersError
  -- Reattach datums and redeemers before evaluating ex units:
  let attachedTx = reattachDatumsAndRedeemers reindexedUnattachedTx
  -- Evaluate transaction ex units:
  rdmrPtrExUnitsList <- evalTxExecutionUnits attachedTx reindexedUnattachedTx
  let
    -- Set execution units received from the server:
    reindexedUnattachedTxWithExUnits =
      updateTxExecutionUnits reindexedUnattachedTx rdmrPtrExUnitsList
  -- Attach datums and redeemers, set the script integrity hash:
  FinalizedTransaction finalizedTx <- lift $
    finalizeTransaction reindexedUnattachedTxWithExUnits
  -- Calculate the minimum fee for a transaction:
  minFee <- ExceptT $ QueryM.calculateMinFee finalizedTx <#> pure <<< unwrap
  pure $ reindexedUnattachedTxWithExUnits /\ minFee

newtype FinalizedTransaction = FinalizedTransaction Transaction

derive instance Generic FinalizedTransaction _
derive instance Newtype FinalizedTransaction _
derive newtype instance Eq FinalizedTransaction

instance Show FinalizedTransaction where
  show = genericShow

-- | Attaches datums and redeemers, sets the script integrity hash,
-- | for use after reindexing.
finalizeTransaction
  :: UnattachedUnbalancedTx -> QueryM FinalizedTransaction
finalizeTransaction reindexedUnattachedTxWithExUnits =
  let
    attachedTxWithExUnits =
      reattachDatumsAndRedeemers reindexedUnattachedTxWithExUnits
    ws = attachedTxWithExUnits ^. _witnessSet # unwrap
    redeemers = fromMaybe mempty ws.redeemers
    datums = wrap <$> fromMaybe mempty ws.plutusData
  in
    do
      costModels <- asks (_.runtime >>> _.pparams >>> unwrap >>> _.costModels)
      liftEffect $ FinalizedTransaction <$>
        setScriptDataHash costModels redeemers datums attachedTxWithExUnits

reindexRedeemers
  :: UnattachedUnbalancedTx
  -> QueryM (Either ReindexErrors UnattachedUnbalancedTx)
reindexRedeemers
  unattachedTx@(UnattachedUnbalancedTx { redeemersTxIns }) =
  let
    inputs = Array.fromFoldable $ unattachedTx ^. _body' <<< _inputs
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
  :: UnattachedUnbalancedTx
  -> Ogmios.TxEvaluationResult
  -> UnattachedUnbalancedTx
updateTxExecutionUnits unattachedTx rdmrPtrExUnitsList =
  unattachedTx #
    _redeemersTxIns %~ flip setRdmrsExecutionUnits rdmrPtrExUnitsList

setRdmrsExecutionUnits
  :: Array (Redeemer /\ Maybe TransactionInput)
  -> Ogmios.TxEvaluationResult
  -> Array (Redeemer /\ Maybe TransactionInput)
setRdmrsExecutionUnits rs (Ogmios.TxEvaluationResult xxs) =
  case Array.uncons (Map.toUnfoldable xxs) of
    Nothing -> rs
    Just { head: ptr /\ exUnits, tail: xs } ->
      let
        xsWrapped = Ogmios.TxEvaluationResult (Map.fromFoldable xs)
        ixMaybe = flip Array.findIndex rs $ \(Redeemer rdmr /\ _) ->
          rdmr.tag == ptr.redeemerTag
            && rdmr.index == Natural.toBigInt ptr.redeemerIndex
      in
        ixMaybe # maybe (setRdmrsExecutionUnits rs xsWrapped) \ix ->
          flip setRdmrsExecutionUnits xsWrapped $
            rs # Lens.ix ix %~ \(Redeemer rec /\ txOutRef) ->
              let
                mem = Natural.toBigInt exUnits.memory
                steps = Natural.toBigInt exUnits.steps
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
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTx unbalancedTx = do
  QueryM.getWalletAddress >>= case _ of
    Nothing ->
      pure $ Left CouldNotGetWalletAddress
    Just address ->
      balanceTxWithAddress address unbalancedTx

-- | Like `balanceTx`, but allows to provide an address that is treated like
-- | user's own (while `balanceTx` gets it from the wallet).
balanceTxWithAddress
  :: Address
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithAddress ownAddr unbalancedTx = runExceptT do
  utxos <- ExceptT $ utxosAt ownAddr
    <#> note CouldNotGetUtxos >>> map unwrap

  unbalancedCollTx <-
    case Array.null (unbalancedTx ^. _redeemersTxIns) of
      true ->
        -- Don't set collateral if tx doesn't contain phase-2 scripts:
        lift unbalancedTxWithNetworkId
      false ->
        setTransactionCollateral =<< lift unbalancedTxWithNetworkId

  let
    allUtxos :: Utxos
    allUtxos =
      -- Combine utxos at the user address and those from any scripts
      -- involved with the contract in the unbalanced transaction:
      utxos `Map.union` (unbalancedTx ^. _unbalancedTx <<< _utxoIndex)

  availableUtxos <- lift $ filterLockedUtxos allUtxos

  logTx "unbalancedCollTx" availableUtxos unbalancedCollTx

  -- Balance and finalize the transaction: 
  ExceptT $ runBalancer availableUtxos ownAddr
    (unbalancedTx # _transaction' .~ unbalancedCollTx)
  where
  unbalancedTxWithNetworkId :: QueryM Transaction
  unbalancedTxWithNetworkId = do
    let transaction = unbalancedTx ^. _transaction'
    networkId <-
      transaction ^. _body <<< _networkId #
        maybe (asks $ _.networkId <<< _.config) pure
    pure (transaction # _body <<< _networkId ?~ networkId)

  setTransactionCollateral :: Transaction -> BalanceTxM Transaction
  setTransactionCollateral transaction = do
    collateral <-
      ExceptT $ note CouldNotGetCollateral <<< map (map (_.input <<< unwrap))
        <$> getWalletCollateral
    pure $ transaction # _body <<< _collateral ?~ collateral

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type ChangeAddress = Address
type TransactionChangeOutput = TransactionOutput
type MinFee = BigInt
type Iteration = Int

runBalancer
  :: Utxos
  -> ChangeAddress
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
runBalancer utxos changeAddress =
  runExceptT <<<
    (mainLoop one zero <=< addLovelacesToTransactionOutputs)
  where
  mainLoop
    :: Iteration
    -> MinFee
    -> UnattachedUnbalancedTx
    -> BalanceTxM FinalizedTransaction
  mainLoop iteration minFee unbalancedTx = do
    let
      unbalancedTxWithMinFee =
        setTransactionMinFee minFee unbalancedTx

    unbalancedTxWithInputs <-
      addTransactionInputs changeAddress utxos unbalancedTxWithMinFee

    traceMainLoop "added transaction inputs" "unbalancedTxWithInputs"
      unbalancedTxWithInputs

    let
      prebalancedTx =
        addTransactionChangeOutput changeAddress utxos unbalancedTxWithInputs

    traceMainLoop "added transaction change output" "prebalancedTx"
      prebalancedTx

    balancedTx /\ newMinFee <- evalExUnitsAndMinFee prebalancedTx

    traceMainLoop "calculated ex units and min fee" "balancedTx" balancedTx

    case newMinFee == minFee of
      true -> do
        finalizedTransaction <- lift $ finalizeTransaction balancedTx

        traceMainLoop "finalized transaction" "finalizedTransaction"
          finalizedTransaction
        pure finalizedTransaction
      false ->
        mainLoop (iteration + one) newMinFee unbalancedTxWithInputs
    where
    traceMainLoop
      :: forall (a :: Type). Show a => String -> String -> a -> BalanceTxM Unit
    traceMainLoop meta message object =
      let
        tagMessage :: String
        tagMessage =
          "mainLoop (iteration " <> show iteration <> "): " <> meta
      in
        Logger.trace (tag tagMessage "^") $ message <> ": " <> show object

setTransactionMinFee
  :: MinFee -> UnattachedUnbalancedTx -> UnattachedUnbalancedTx
setTransactionMinFee minFee = _body' <<< _fee .~ Coin minFee

-- | For each transaction output, if necessary, adds some number of lovelaces
-- | to cover the utxo min-ada-value requirement.
addLovelacesToTransactionOutputs
  :: UnattachedUnbalancedTx -> BalanceTxM UnattachedUnbalancedTx
addLovelacesToTransactionOutputs unbalancedTx =
  map (\txOutputs -> unbalancedTx # _body' <<< _outputs .~ txOutputs) $
    traverse addLovelacesToTransactionOutput
      (unbalancedTx ^. _body' <<< _outputs)

addLovelacesToTransactionOutput
  :: TransactionOutput -> BalanceTxM TransactionOutput
addLovelacesToTransactionOutput txOutput = do
  txOutputMinAda <- ExceptT $ utxoMinAdaValue txOutput
    <#> note UtxoMinAdaValueCalculationFailed
  let
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = Coin $ max (valueToCoin' txOutputValue) txOutputMinAda

  pure $ wrap txOutputRec
    { amount = mkValue newCoin (getNonAdaAsset txOutputValue) }

-- | Generates a change output to return all excess `Value` back to the owner's 
-- | address. Does NOT check if the generated output fulfills the utxo
-- | min-ada-value requirement (see Prerequisites).
-- | 
-- | Prerequisites: 
-- |   1. Must be called after `addTransactionInputs`, which guarantees that 
-- |   the change output will cover the utxo min-ada-value requirement.
-- | 
-- | TODO: Modify the logic to handle "The Problem of Concurrency"
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/924
buildTransactionChangeOutput
  :: ChangeAddress -> Utxos -> UnattachedUnbalancedTx -> TransactionChangeOutput
buildTransactionChangeOutput changeAddress utxos tx =
  let
    txBody :: TxBody
    txBody = tx ^. _body'

    totalInputValue :: Value
    totalInputValue = getInputValue utxos txBody

    changeValue :: Value
    changeValue =
      valueWithNonNegativeAda $
        (totalInputValue <> mintValue txBody)
          `minus` (totalOutputValue txBody <> minFeeValue txBody)
  in
    TransactionOutput
      { address: changeAddress, amount: changeValue, dataHash: Nothing }

addTransactionChangeOutput
  :: ChangeAddress -> Utxos -> UnattachedUnbalancedTx -> UnattachedUnbalancedTx
addTransactionChangeOutput changeAddress utxos unbalancedTx =
  unbalancedTx # _body' <<< _outputs %~
    Array.cons (buildTransactionChangeOutput changeAddress utxos unbalancedTx)

-- | Selects a combination of unspent transaction outputs from the wallet's 
-- | utxo set so that the total input value is sufficient to cover all  
-- | transaction outputs, including the change that will be generated 
-- | when using that particular combination of inputs. 
-- | 
-- | Prerequisites:
-- |   1. Must be called with a transaction with no change output. 
-- |   2. The `fee` field of a transaction body must be set.
addTransactionInputs
  :: ChangeAddress
  -> Utxos
  -> UnattachedUnbalancedTx
  -> BalanceTxM UnattachedUnbalancedTx
addTransactionInputs changeAddress utxos unbalancedTx = do
  let
    txBody :: TxBody
    txBody = unbalancedTx ^. _body'

    txInputs :: Set TransactionInput
    txInputs = txBody ^. _inputs

    nonMintedValue :: Value
    nonMintedValue = totalOutputValue txBody `minus` mintValue txBody

  txChangeOutput <-
    addLovelacesToTransactionOutput
      (buildTransactionChangeOutput changeAddress utxos unbalancedTx)

  let
    changeValue :: Value
    changeValue = (unwrap txChangeOutput).amount

    requiredInputValue :: Value
    requiredInputValue = nonMintedValue <> minFeeValue txBody <> changeValue

  newTxInputs <-
    except $ collectTransactionInputs txInputs utxos requiredInputValue

  case newTxInputs == txInputs of
    true ->
      pure unbalancedTx
    false ->
      addTransactionInputs changeAddress utxos
        (unbalancedTx # _body' <<< _inputs %~ Set.union newTxInputs)

collectTransactionInputs
  :: Set TransactionInput
  -> Utxos
  -> Value
  -> Either BalanceTxError (Set TransactionInput)
collectTransactionInputs originalTxIns utxos value = do
  txInsValue <- updatedInputs >>= getTxInsValue utxos
  updatedInputs' <- updatedInputs
  case isSufficient updatedInputs' txInsValue of
    true ->
      pure $ Set.fromFoldable updatedInputs'
    false ->
      Left $ InsufficientTxInputs (Expected value) (Actual txInsValue)
  where
  updatedInputs :: Either BalanceTxError (Array TransactionInput)
  updatedInputs =
    foldl
      ( \newTxIns txIn -> do
          txIns <- newTxIns
          txInsValue <- getTxInsValue utxos txIns
          case Array.elem txIn txIns || isSufficient txIns txInsValue of
            true -> newTxIns
            false ->
              Right $ Array.insert txIn txIns -- treat as a set.
      )
      (Right $ Array.fromFoldable originalTxIns)
      $ utxosToTransactionInput utxos

  isSufficient :: Array TransactionInput -> Value -> Boolean
  isSufficient txIns' txInsValue =
    not (Array.null txIns') && txInsValue `geq` value

  getTxInsValue
    :: Utxos -> Array TransactionInput -> Either BalanceTxError Value
  getTxInsValue utxos' =
    map (Array.foldMap getAmount) <<<
      traverse (\x -> note (UtxoLookupFailedFor x) $ Map.lookup x utxos')

  utxosToTransactionInput :: Utxos -> Array TransactionInput
  utxosToTransactionInput =
    Array.mapMaybe (hush <<< getPublicKeyTransactionInput) <<< Map.toUnfoldable

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

totalOutputValue :: TxBody -> Value
totalOutputValue txBody = foldMap getAmount (txBody ^. _outputs)

mintValue :: TxBody -> Value
mintValue txBody = maybe mempty (mkValue mempty <<< unwrap) (txBody ^. _mint)

minFeeValue :: TxBody -> Value
minFeeValue txBody = mkValue (txBody ^. _fee) mempty

valueWithNonNegativeAda :: Value -> Value
valueWithNonNegativeAda value =
  mkValue (Coin $ max (valueToCoin' value) zero) (getNonAdaAsset value)

-- | Get `TransactionInput` such that it is associated to `PaymentCredentialKey`
-- | and not `PaymentCredentialScript`, i.e. we want wallets only
getPublicKeyTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either BalanceTxError TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  note CouldNotConvertScriptOutputToTxInput $ do
    paymentCred <- unwrap txOut # (_.address >>> addressPaymentCred)
    -- TEST ME: using StakeCredential to determine whether wallet or script
    paymentCred # withStakeCredential
      { onKeyHash: const $ pure txOutRef
      , onScriptHash: const Nothing
      }

getInputValue :: Utxos -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    ( Array.mapMaybe (flip Map.lookup utxos)
        <<< Array.fromFoldable
        <<< _.inputs $ txBody
    )

--------------------------------------------------------------------------------
-- Logging Helpers 
--------------------------------------------------------------------------------

-- Logging for Transaction type without returning Transaction
logTx
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadLogger m
  => String
  -> Utxos
  -> Transaction
  -> m Unit
logTx msg utxos (Transaction { body: body'@(TxBody body) }) =
  traverse_ (Logger.trace (tag msg mempty))
    [ "Input Value: " <> show (getInputValue utxos body')
    , "Output Value: " <> show (Array.foldMap getAmount body.outputs)
    , "Fees: " <> show body.fee
    ]

