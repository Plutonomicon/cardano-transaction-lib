module BalanceTx
  ( Actual(Actual)
  , AddTransactionInputsError(CannotBuildNonMintedValue)
  , BalanceTxError
      ( AddTransactionInputsError'
      , BuildTxChangeOutputError'
      , GetWalletAddressError'
      , GetWalletCollateralError'
      , UtxosAtError'
      , UtxoMinAdaValueCalcError'
      , GetPublicKeyTransactionInputError'
      , BalanceTxInsError'
      , EvalExUnitsAndMinFeeError'
      )
  , BalanceTxInsError
      ( InsufficientTxInputs
      , UtxoLookupFailedFor
      )
  , BuildTxChangeOutputError(CannotBuildChangeValue)
  , EvalExUnitsAndMinFeeError
      ( EvalTxFailure
      , ReindexRedeemersError
      )
  , Expected(Expected)
  , FinalizedTransaction(FinalizedTransaction)
  , GetPublicKeyTransactionInputError(CannotConvertScriptOutputToTxInput)
  , GetWalletAddressError(CouldNotGetWalletAddress)
  , GetWalletCollateralError
      ( CouldNotGetCollateral
      )
  , UtxosAtError(CouldNotGetUtxos)
  , UtxoMinAdaValueCalcError(UtxoMinAdaValueCalcError)
  , balanceTx
  , balanceTxWithAddress
  , printTxEvaluationFailure
  ) where

import Prelude

import BalanceTx.UtxoMinAda (adaOnlyUtxoMinAdaValue, utxoMinAdaValue)
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
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
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
import Data.Array (filter, catMaybes)
import Data.Array as Array
import Data.Bifunctor (lmap, bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), hush, note, either, isLeft)
import Data.Foldable (find, foldl, length, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (applyN)
import Data.Int (toStringAs, decimal, ceil, toNumber)
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
import Data.String (Pattern(Pattern))
import Data.String.Common (joinWith, split) as String
import Data.String.CodePoints (length) as String
import Data.String.Utils (padEnd)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import QueryM (ClientError, QueryM)
import QueryM (evaluateTxOgmios, getWalletAddress) as QueryM
import QueryM.MinFee (calculateMinFee) as QueryM
import QueryM.Ogmios
  ( TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , RedeemerPointer
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , IllFormedExecutionBudget
      , NoCostModelForLanguage
      )
  ) as Ogmios
import QueryM.Utxos (utxosAt, filterLockedUtxos, getWalletCollateral)
import ReindexRedeemers (ReindexErrors, reindexSpentScriptRedeemers')
import Serialization (convertTransaction, toBytes) as Serialization
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import Transaction (setScriptDataHash)
import Types.Natural (toBigInt) as Natural
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (UnbalancedTx(UnbalancedTx), _transaction)
import Untagged.Union (asOneOf)

--------------------------------------------------------------------------------
-- Errors for Balancing functions
--------------------------------------------------------------------------------

-- These derivations may need tweaking when testing to make sure they are easy
-- to read, especially with generic show vs newtype show derivations.
data BalanceTxError
  = AddTransactionInputsError' AddTransactionInputsError
  | BalanceTxInsError' BalanceTxInsError
  | BuildTxChangeOutputError' BuildTxChangeOutputError
  | EvalExUnitsAndMinFeeError' EvalExUnitsAndMinFeeError
  | GetPublicKeyTransactionInputError' GetPublicKeyTransactionInputError
  | GetWalletAddressError' GetWalletAddressError
  | GetWalletCollateralError' GetWalletCollateralError
  | UtxosAtError' UtxosAtError
  | UtxoMinAdaValueCalcError' UtxoMinAdaValueCalcError

derive instance Generic BalanceTxError _

instance Show BalanceTxError where
  show (EvalExUnitsAndMinFeeError' (EvalTxFailure tx failure)) =
    "EvalExUnitsAndMinFeeError': EvalTxFailure: " <> printTxEvaluationFailure tx
      failure
  show e = genericShow e

type WorkingLine = String
type FrozenLine = String

type PrettyString = Array (Either WorkingLine FrozenLine)

runPrettyString :: PrettyString -> String
runPrettyString ary = String.joinWith "" (either identity identity <$> ary)

freeze :: PrettyString -> PrettyString
freeze ary = either Right Right <$> ary

line :: String -> PrettyString
line s =
  case Array.uncons lines of
    Nothing -> []
    Just { head, tail } -> [ head ] <> freeze tail
  where
  lines = Left <<< (_ <> "\n") <$> String.split (Pattern "\n") s

bullet :: PrettyString -> PrettyString
bullet ary = freeze (bimap ("- " <> _) ("  " <> _) <$> ary)

number :: PrettyString -> PrettyString
number ary = freeze (foldl go [] ary)
  where
  biggestPrefix :: String
  biggestPrefix = toStringAs decimal (length (filter isLeft ary)) <> ". "

  width :: Int
  width = ceil (toNumber (String.length biggestPrefix) / 2.0) * 2

  numberLine :: Int -> String -> String
  numberLine i l = padEnd width (toStringAs decimal (i + 1) <> ". ") <> l

  indentLine :: String -> String
  indentLine = applyN ("  " <> _) (width / 2)

  go :: PrettyString -> Either WorkingLine FrozenLine -> PrettyString
  go b a = b <> [ bimap (numberLine $ length b) indentLine a ]

-- | Pretty print the failure response from Ogmios's EvaluateTx endpoint.
-- | Exported to allow testing, use `Test.Ogmios.Aeson.printEvaluateTxFailures`
-- | to visually verify the printing of errors without a context on fixtures.
printTxEvaluationFailure
  :: UnattachedUnbalancedTx -> Ogmios.TxEvaluationFailure -> String
printTxEvaluationFailure (UnattachedUnbalancedTx { redeemersTxIns }) e =
  runPrettyString $ case e of
    Ogmios.UnparsedError error -> line $ "Unknown error: " <> error
    Ogmios.ScriptFailures sf -> line "Script failures:" <> bullet
      (foldMapWithIndex printScriptFailures sf)
  where
  lookupRedeemerPointer
    :: Ogmios.RedeemerPointer -> Maybe (Redeemer /\ Maybe TransactionInput)
  lookupRedeemerPointer ptr = flip find redeemersTxIns
    $ \(Redeemer rdmr /\ _) -> rdmr.tag == ptr.redeemerTag && rdmr.index ==
        Natural.toBigInt ptr.redeemerIndex

  printRedeemerPointer :: Ogmios.RedeemerPointer -> PrettyString
  printRedeemerPointer ptr =
    line
      ( show ptr.redeemerTag <> ":" <> BigInt.toString
          (Natural.toBigInt ptr.redeemerIndex)
      )

  -- TODO Investigate if more details can be printed, for example minting
  -- policy/minted assets
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/881
  printRedeemerDetails :: Ogmios.RedeemerPointer -> PrettyString
  printRedeemerDetails ptr =
    let
      mbRedeemerTxIn = lookupRedeemerPointer ptr
      mbData = mbRedeemerTxIn <#> \(Redeemer r /\ _) -> "Redeemer: " <> show
        r.data
      mbTxIn = (mbRedeemerTxIn >>= snd) <#> \txIn -> "Input: " <> show txIn
    in
      foldMap line $ catMaybes [ mbData, mbTxIn ]

  printRedeemer :: Ogmios.RedeemerPointer -> PrettyString
  printRedeemer ptr =
    printRedeemerPointer ptr <> bullet (printRedeemerDetails ptr)

  printScriptFailure :: Ogmios.ScriptFailure -> PrettyString
  printScriptFailure = case _ of
    Ogmios.ExtraRedeemers ptrs -> line "Extra redeemers:" <> bullet
      (foldMap printRedeemer ptrs)
    Ogmios.MissingRequiredDatums { provided, missing }
    -> line "Supplied with datums:"
      <> bullet (foldMap (foldMap line) provided)
      <> line "But missing required datums:"
      <> bullet (foldMap line missing)
    Ogmios.MissingRequiredScripts { resolved, missing }
    -> line "Supplied with scripts:"
      <> bullet
        ( foldMapWithIndex
            (\ptr scr -> printRedeemer ptr <> line ("Script: " <> scr))
            resolved
        )
      <> line "But missing required scripts:"
      <> bullet (foldMap line missing)
    Ogmios.ValidatorFailed { error, traces } -> line error <> line "Trace:" <>
      number
        (foldMap line traces)
    Ogmios.UnknownInputReferencedByRedeemer txIn -> line
      ("Unknown input referenced by redeemer: " <> show txIn)
    Ogmios.NonScriptInputReferencedByRedeemer txIn -> line
      ("Non script input referenced by redeemer: " <> show txIn)
    Ogmios.IllFormedExecutionBudget Nothing -> line
      ("Ill formed execution budget: Execution budget missing")
    Ogmios.IllFormedExecutionBudget (Just { memory, steps }) ->
      line "Ill formed execution budget:"
        <> bullet
          ( line ("Memory: " <> BigInt.toString (Natural.toBigInt memory))
              <> line ("Steps: " <> BigInt.toString (Natural.toBigInt steps))
          )
    Ogmios.NoCostModelForLanguage language -> line
      ("No cost model for language \"" <> language <> "\"")

  printScriptFailures
    :: Ogmios.RedeemerPointer -> Array Ogmios.ScriptFailure -> PrettyString
  printScriptFailures ptr sfs = printRedeemer ptr <> bullet
    (foldMap printScriptFailure sfs)

data AddTransactionInputsError = CannotBuildNonMintedValue

derive instance Generic AddTransactionInputsError _

instance Show AddTransactionInputsError where
  show = genericShow

data BuildTxChangeOutputError = CannotBuildChangeValue

derive instance Generic BuildTxChangeOutputError _

instance Show BuildTxChangeOutputError where
  show = genericShow

data GetWalletAddressError = CouldNotGetWalletAddress

derive instance Generic GetWalletAddressError _

instance Show GetWalletAddressError where
  show = genericShow

data GetWalletCollateralError = CouldNotGetCollateral

derive instance Generic GetWalletCollateralError _

instance Show GetWalletCollateralError where
  show = genericShow

data UtxosAtError = CouldNotGetUtxos

derive instance Generic UtxosAtError _

instance Show UtxosAtError where
  show = genericShow

data EvalExUnitsAndMinFeeError
  = ReindexRedeemersError ReindexErrors
  | EvalTxFailure UnattachedUnbalancedTx Ogmios.TxEvaluationFailure

derive instance Generic EvalExUnitsAndMinFeeError _

instance Show EvalExUnitsAndMinFeeError where
  show = genericShow

data GetPublicKeyTransactionInputError = CannotConvertScriptOutputToTxInput

derive instance Generic GetPublicKeyTransactionInputError _

instance Show GetPublicKeyTransactionInputError where
  show = genericShow

data BalanceTxInsError
  = InsufficientTxInputs Expected Actual
  | UtxoLookupFailedFor TransactionInput

derive instance Generic BalanceTxInsError _

instance Show BalanceTxInsError where
  show = genericShow

newtype Expected = Expected Value

derive instance Generic Expected _
derive instance Newtype Expected _

instance Show Expected where
  show = genericShow

newtype Actual = Actual Value

derive instance Generic Actual _
derive instance Newtype Actual _

instance Show Actual where
  show = genericShow

data UtxoMinAdaValueCalcError = UtxoMinAdaValueCalcError

derive instance Generic UtxoMinAdaValueCalcError _

instance Show UtxoMinAdaValueCalcError where
  show = genericShow

--------------------------------------------------------------------------------
-- Evaluation of fees and execution units, Updating redeemers
--------------------------------------------------------------------------------

evalTxExecutionUnits
  :: Transaction
  -> UnattachedUnbalancedTx
  -> QueryM (Either EvalExUnitsAndMinFeeError Ogmios.TxEvaluationResult)
evalTxExecutionUnits tx unattachedTx = do
  txBytes <- liftEffect
    ( wrap <<< Serialization.toBytes <<< asOneOf <$>
        Serialization.convertTransaction tx
    )
  lmap (EvalTxFailure unattachedTx) <<< unwrap <$> QueryM.evaluateTxOgmios
    txBytes

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
    reindexedUnattachedTx <- ExceptT $ reindexRedeemers unattachedTx
      <#> lmap ReindexRedeemersError
    -- Reattach datums and redeemers before evaluating ex units:
    let attachedTx = reattachDatumsAndRedeemers reindexedUnattachedTx
    -- Evaluate transaction ex units:
    rdmrPtrExUnitsList <- ExceptT $ evalTxExecutionUnits attachedTx
      reindexedUnattachedTx
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

evalExUnitsAndMinFee
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError (UnattachedUnbalancedTx /\ BigInt))
evalExUnitsAndMinFee =
  map (lmap EvalExUnitsAndMinFeeError') <<< evalExUnitsAndMinFee'

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
-- Setting collateral
--------------------------------------------------------------------------------

setCollateral
  :: Transaction
  -> QueryM (Either GetWalletCollateralError Transaction)
setCollateral transaction = runExceptT do
  collateral <- ExceptT $ getWalletCollateral <#> note CouldNotGetCollateral
  pure $ addTxCollateral collateral transaction

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral utxos transaction =
  transaction # _body <<< _collateral ?~ map (_.input <<< unwrap) utxos

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
balanceTx tx = do
  QueryM.getWalletAddress >>= case _ of
    Nothing -> pure $ Left $ GetWalletAddressError' CouldNotGetWalletAddress
    Just address -> balanceTxWithAddress address tx

-- | Like `balanceTx`, but allows to provide an address that is treated like
-- | user's own (while `balanceTx` gets it from the wallet).
balanceTxWithAddress
  :: Address
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithAddress
  ownAddr
  unattachedTx@(UnattachedUnbalancedTx { unbalancedTx: t }) = do
  let (UnbalancedTx { transaction: unbalancedTx, utxoIndex }) = t
  networkId <- (unbalancedTx ^. _body <<< _networkId) #
    maybe (asks $ _.config >>> _.networkId) pure
  let unbalancedTx' = unbalancedTx # _body <<< _networkId ?~ networkId
  runExceptT do
    -- Get own wallet address, collateral and utxo set:
    utxos <- ExceptT $ utxosAt ownAddr <#>
      (note (UtxosAtError' CouldNotGetUtxos) >>> map unwrap)

    -- After adding collateral, we need to balance the inputs and
    -- non-Ada outputs before looping, i.e. we need to add input fees
    -- for the Ada only collateral. No MinUtxos required. Perhaps
    -- for some wallets this step can be skipped and we can go straight
    -- to prebalancer.
    unbalancedCollTx <-
      if Array.null (unattachedTx ^. _redeemersTxIns)
      -- Don't set collateral if tx doesn't contain phase-2 scripts:
      then pure unbalancedTx'
      else ExceptT $ setCollateral unbalancedTx'
        <#> lmap GetWalletCollateralError'

    let
      -- Combines utxos at the user address and those from any scripts
      -- involved with the contract in the unbalanced transaction.
      allUtxos :: Utxos
      allUtxos = utxos `Map.union` utxoIndex

    availableUtxos <- lift $ filterLockedUtxos allUtxos

    -- Logging Unbalanced Tx with collateral added:
    logTx "Unbalanced Collaterised Tx " availableUtxos unbalancedCollTx

    finalizedTx <- ExceptT $
      runBalancer availableUtxos ownAddr
        (unattachedTx # _transaction' .~ unbalancedCollTx)

    -- Log final balanced tx and return it:
    logTx "Post-balancing Tx " availableUtxos (unwrap finalizedTx)
    except $ Right finalizedTx

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

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type ChangeAddress = Address
type TransactionChangeOutput = TransactionOutput
type MinFee = BigInt

runBalancer
  :: Utxos
  -> ChangeAddress
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
runBalancer utxos changeAddress unbalancedTx' = runExceptT do
  (ExceptT <<< mainLoop zero)
    =<< (ExceptT $ addLovelacesToTransactionOutputs unbalancedTx')
  where
  mainLoop
    :: MinFee
    -> UnattachedUnbalancedTx
    -> QueryM (Either BalanceTxError FinalizedTransaction)
  mainLoop minFee unbalancedTx = runExceptT do
    let
      unbalancedTxWithMinFee :: UnattachedUnbalancedTx
      unbalancedTxWithMinFee = setTransactionMinFee minFee unbalancedTx

    unbalancedTxWithInputs <-
      ExceptT $ addTransactionInputs changeAddress utxos
        unbalancedTxWithMinFee

    prebalancedTx <-
      ExceptT $ addTransactionChangeOutput changeAddress utxos
        unbalancedTxWithInputs

    balancedTx /\ newMinFee <-
      ExceptT $ evalExUnitsAndMinFee prebalancedTx

    case newMinFee == minFee of
      true ->
        lift $ finalizeTransaction balancedTx
      false ->
        ExceptT $ mainLoop newMinFee unbalancedTxWithInputs

setTransactionMinFee
  :: MinFee -> UnattachedUnbalancedTx -> UnattachedUnbalancedTx
setTransactionMinFee minFee = _body' <<< _fee .~ Coin minFee

addLovelacesToTransactionOutputs
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
addLovelacesToTransactionOutputs unbalancedTx = runExceptT do
  map (\txOutputs -> unbalancedTx # _body' <<< _outputs .~ txOutputs) $
    traverse (ExceptT <<< addLovelacesToTransactionOutput)
      (unbalancedTx ^. _body' <<< _outputs)

addLovelacesToTransactionOutput
  :: TransactionOutput -> QueryM (Either BalanceTxError TransactionOutput)
addLovelacesToTransactionOutput txOutput = runExceptT do
  txOutputMinAda <- ExceptT $ utxoMinAdaValue txOutput
    <#> note (UtxoMinAdaValueCalcError' UtxoMinAdaValueCalcError)
  let
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = Coin $ max (valueToCoin' txOutputValue) txOutputMinAda

  pure $ wrap txOutputRec
    { amount = mkValue newCoin (getNonAdaAsset txOutputValue) }

buildTransactionChangeOutput
  :: ChangeAddress
  -> Utxos
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError TransactionChangeOutput)
buildTransactionChangeOutput changeAddress utxos tx = runExceptT do
  let
    txBody :: TxBody
    txBody = tx ^. _body'

    totalInputValue :: Value
    totalInputValue = getInputValue utxos txBody

  changeValue <- except $
    note (BuildTxChangeOutputError' CannotBuildChangeValue)
      ( (totalInputValue <> mintValue txBody)
          `minus` (totalOutputValue txBody <> minFeeValue txBody)
      )
  let
    amount :: Value
    amount = valueWithNonNegativeAda changeValue

  pure $ TransactionOutput
    { address: changeAddress, amount, dataHash: Nothing }

addTransactionChangeOutput
  :: ChangeAddress
  -> Utxos
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
addTransactionChangeOutput changeAddress utxos unbalancedTx =
  map (\change -> unbalancedTx # _body' <<< _outputs %~ Array.cons change)
    <$> buildTransactionChangeOutput changeAddress utxos unbalancedTx

addTransactionInputs
  :: ChangeAddress
  -> Utxos
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
addTransactionInputs changeAddress utxos unbalancedTx = runExceptT do
  let
    txBody :: TxBody
    txBody = unbalancedTx ^. _body'

    txInputs :: Set TransactionInput
    txInputs = txBody ^. _inputs

  nonMintedValue <- except $
    note (AddTransactionInputsError' CannotBuildNonMintedValue)
      (totalOutputValue txBody `minus` mintValue txBody)

  txChangeOutput <-
    ExceptT (buildTransactionChangeOutput changeAddress utxos unbalancedTx)
      >>= (ExceptT <<< addLovelacesToTransactionOutput)

  let
    changeValue :: Value
    changeValue = (unwrap txChangeOutput).amount

    requiredInputValue :: Value
    requiredInputValue = nonMintedValue <> minFeeValue txBody <> changeValue

  newTxInputs <- except $ lmap BalanceTxInsError' $
    collectTransactionInputs txInputs utxos requiredInputValue

  case newTxInputs == txInputs of
    true -> pure unbalancedTx
    false -> ExceptT $
      addTransactionInputs changeAddress utxos
        (unbalancedTx # _body' <<< _inputs %~ Set.union newTxInputs)

collectTransactionInputs
  :: Set TransactionInput
  -> Utxos
  -> Value
  -> Either BalanceTxInsError (Set TransactionInput)
collectTransactionInputs originalTxIns utxos value = do
  txInsValue <- updatedInputs >>= getTxInsValue utxos
  updatedInputs' <- updatedInputs
  case isSufficient updatedInputs' txInsValue of
    true ->
      pure $ Set.fromFoldable updatedInputs'
    false ->
      Left $ InsufficientTxInputs (Expected value) (Actual txInsValue)
  where
  updatedInputs :: Either BalanceTxInsError (Array TransactionInput)
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
    :: Utxos -> Array TransactionInput -> Either BalanceTxInsError Value
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
  -> Either GetPublicKeyTransactionInputError TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  note CannotConvertScriptOutputToTxInput $ do
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

