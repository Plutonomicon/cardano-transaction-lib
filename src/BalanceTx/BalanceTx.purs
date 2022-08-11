module BalanceTx
  ( Actual(Actual)
  , AddTxCollateralsError
      ( CollateralUtxosUnavailable
      , AddTxCollateralsError
      )
  , BalanceNonAdaOutsError
      ( InputsCannotBalanceNonAdaTokens
      , BalanceNonAdaOutsCannotMinus
      )
  , BalanceTxError
      ( GetWalletAddressError'
      , GetWalletCollateralError'
      , UtxosAtError'
      , UtxoMinAdaValueCalcError'
      , ReturnAdaChangeError'
      , AddTxCollateralsError'
      , GetPublicKeyTransactionInputError'
      , BalanceTxInsError'
      , BalanceNonAdaOutsError'
      , EvalExUnitsAndMinFeeError'
      , TxInputLockedError'
      )
  , BalanceTxInsError
      ( InsufficientTxInputs
      , BalanceTxInsCannotMinus
      , UtxoLookupFailedFor
      )
  , CannotMinusError(CannotMinus)
  , EvalExUnitsAndMinFeeError
      ( EvalMinFeeError
      , ReindexRedeemersError
      , EvalTxFailure
      )
  , Expected(Expected)
  , FinalizedTransaction(FinalizedTransaction)
  , GetPublicKeyTransactionInputError(CannotConvertScriptOutputToTxInput)
  , GetWalletAddressError(CouldNotGetWalletAddress)
  , GetWalletCollateralError
      ( CouldNotGetCollateral
      )
  , TxInputLockedError(TxInputLockedError)
  , ImpossibleError(Impossible)
  , ReturnAdaChangeError
      ( ReturnAdaChangeError
      , ReturnAdaChangeImpossibleError
      , ReturnAdaChangeCalculateMinFee
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
  , _networkId
  , _outputs
  , _plutusData
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value
  ( Value
  , filterNonAda
  , geq
  , getLovelace
  , isPos
  , isZero
  , lovelaceValueOf
  , minus
  , mkCoin
  , mkValue
  , valueToCoin
  , valueToCoin'
  )
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array ((\\), modifyAt, filter, catMaybes)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt, fromInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), hush, note, either, isLeft)
import Data.Foldable (find, foldl, length, foldMap)
import Data.Foldable (lookup) as Foldable
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (applyN)
import Data.Int (toStringAs, decimal, ceil, toNumber)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens')
import Data.Lens.Getter ((^.))
import Data.Lens.Index (ix) as Lens
import Data.Lens.Setter ((.~), set, (?~), (%~))
import Data.List ((:), List(Nil), partition)
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
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import QueryM (ClientError, QueryM)
import QueryM
  ( calculateMinFee
  , getWalletAddress
  , evaluateTxOgmios
  ) as QueryM
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
  | TxInputLockedError' TxInputLockedError
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
  = EvalMinFeeError ClientError
  | ReindexRedeemersError ReindexErrors
  | EvalTxFailure UnattachedUnbalancedTx Ogmios.TxEvaluationFailure

derive instance Generic EvalExUnitsAndMinFeeError _

instance Show EvalExUnitsAndMinFeeError where
  show = genericShow

data ReturnAdaChangeError
  = ReturnAdaChangeError String
  | ReturnAdaChangeImpossibleError String ImpossibleError
  | ReturnAdaChangeCalculateMinFee EvalExUnitsAndMinFeeError

derive instance Generic ReturnAdaChangeError _

instance Show ReturnAdaChangeError where
  show = genericShow

data AddTxCollateralsError
  = CollateralUtxosUnavailable
  | AddTxCollateralsError

derive instance Generic AddTxCollateralsError _

instance Show AddTxCollateralsError where
  show = genericShow

data GetPublicKeyTransactionInputError = CannotConvertScriptOutputToTxInput

derive instance Generic GetPublicKeyTransactionInputError _

instance Show GetPublicKeyTransactionInputError where
  show = genericShow

data BalanceTxInsError
  = InsufficientTxInputs Expected Actual
  | BalanceTxInsCannotMinus CannotMinusError
  | UtxoLookupFailedFor TransactionInput

derive instance Generic BalanceTxInsError _

instance Show BalanceTxInsError where
  show = genericShow

data CannotMinusError = CannotMinus Actual

derive instance Generic CannotMinusError _

instance Show CannotMinusError where
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

data BalanceNonAdaOutsError
  = InputsCannotBalanceNonAdaTokens
  | BalanceNonAdaOutsCannotMinus CannotMinusError

derive instance Generic BalanceNonAdaOutsError _

instance Show BalanceNonAdaOutsError where
  show = genericShow

data TxInputLockedError = TxInputLockedError

derive instance Generic TxInputLockedError _

instance Show TxInputLockedError where
  show = genericShow

data UtxoMinAdaValueCalcError = UtxoMinAdaValueCalcError

derive instance Generic UtxoMinAdaValueCalcError _

instance Show UtxoMinAdaValueCalcError where
  show = genericShow

-- | Represents that an error reason should be impossible
data ImpossibleError = Impossible

derive instance Generic ImpossibleError _

instance Show ImpossibleError where
  show = genericShow

--------------------------------------------------------------------------------
-- Newtype wrappers, Type aliases, Temporary placeholder types
--------------------------------------------------------------------------------

-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

newtype FinalizedTransaction = FinalizedTransaction Transaction

derive instance Generic FinalizedTransaction _
derive instance Newtype FinalizedTransaction _
derive newtype instance Eq FinalizedTransaction

instance Show FinalizedTransaction where
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
    minFee <- ExceptT $ QueryM.calculateMinFee finalizedTx
      <#> bimap EvalMinFeeError unwrap
    pure $ reindexedUnattachedTxWithExUnits /\ minFee

evalExUnitsAndMinFee
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError (UnattachedUnbalancedTx /\ BigInt))
evalExUnitsAndMinFee =
  map (lmap EvalExUnitsAndMinFeeError') <<< evalExUnitsAndMinFee'

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
  utxoMinVal <- adaOnlyUtxoMinAdaValue
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

    -- Prebalance collaterised tx without fees:
    ubcTx <- except $
      prebalanceCollateral zero availableUtxos utxoMinVal unbalancedCollTx
    -- Prebalance collaterised tx with fees:
    let unattachedTx' = unattachedTx # _transaction' .~ ubcTx
    _ /\ fees <- ExceptT $ evalExUnitsAndMinFee unattachedTx'
    ubcTx' <- except $
      prebalanceCollateral (fees + feeBuffer) availableUtxos utxoMinVal
        ubcTx
    -- Loop to balance non-Ada assets
    nonAdaBalancedCollTx <- ExceptT $ loop availableUtxos ownAddr [] $
      unattachedTx' #
        _transaction' .~ ubcTx'
    -- Return excess Ada change to wallet:
    unsignedTx <- ExceptT $
      returnAdaChangeAndFinalizeFees ownAddr allUtxos nonAdaBalancedCollTx
        <#>
          lmap ReturnAdaChangeError'
    -- Attach datums and redeemers, set the script integrity hash:
    finalizedTx <- lift $ finalizeTransaction unsignedTx
    -- Log final balanced tx and return it:
    logTx "Post-balancing Tx " availableUtxos (unwrap finalizedTx)
    except $ Right finalizedTx
  where
  prebalanceCollateral
    :: BigInt
    -> Utxos
    -> BigInt
    -> Transaction
    -> Either BalanceTxError Transaction
  prebalanceCollateral fees utxos adaOnlyUtxoMinValue tx =
    balanceTxIns utxos fees adaOnlyUtxoMinValue (tx ^. _body)
      >>= balanceNonAdaOuts ownAddr utxos
      <#> flip (set _body) tx

  loop
    :: Utxos
    -> Address
    -> MinUtxos
    -> UnattachedUnbalancedTx
    -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
  loop utxoIndex' ownAddr' prevMinUtxos' unattachedTx' = runExceptT do
    let
      Transaction { body: txBody'@(TxBody txB) } =
        unattachedTx' ^. _transaction'

    nextMinUtxos' :: MinUtxos <-
      ExceptT $ calculateMinUtxos (txB.outputs \\ map fst prevMinUtxos')
        <#> lmap UtxoMinAdaValueCalcError'

    let
      minUtxos' :: MinUtxos
      minUtxos' = prevMinUtxos' <> nextMinUtxos'

    unattachedTxWithBalancedBody <-
      ExceptT $ chainedBalancer minUtxos' utxoIndex' ownAddr' unattachedTx'

    let balancedTxBody = unattachedTxWithBalancedBody ^. _body'

    if txBody' == balancedTxBody then
      pure unattachedTxWithBalancedBody
    else
      ExceptT $
        loop utxoIndex' ownAddr' minUtxos' unattachedTxWithBalancedBody

  chainedBalancer
    :: MinUtxos
    -> Utxos
    -> Address
    -> UnattachedUnbalancedTx
    -> QueryM (Either BalanceTxError UnattachedUnbalancedTx)
  chainedBalancer minUtxos' utxoIndex' ownAddr' unattachedTx' =
    adaOnlyUtxoMinAdaValue >>= \utxoMinVal -> runExceptT do
      let Transaction tx@{ body: txBody' } = unattachedTx' ^. _transaction'
      txBodyWithoutFees' <- except $
        preBalanceTxBody minUtxos' zero utxoIndex' ownAddr' utxoMinVal txBody'
      let
        tx' = wrap tx { body = txBodyWithoutFees' }
        unattachedTx'' = unattachedTx' # _unbalancedTx <<< _transaction .~ tx'
      unattachedTx''' /\ fees' <- ExceptT $
        evalExUnitsAndMinFee unattachedTx''
      let feesWithBuffer = fees' + feeBuffer
      except <<< map (\body -> unattachedTx''' # _body' .~ body) $
        preBalanceTxBody minUtxos' feesWithBuffer utxoIndex' ownAddr' utxoMinVal
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

-- Transaction should be pre-balanced at this point, and the Ada value of the
-- inputs should be greater than or equal to the value of the outputs.
-- This should be called with a Tx with min Ada in each output utxo,
-- namely, after "loop".
returnAdaChangeAndFinalizeFees
  :: Address
  -> Utxos
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
        ExceptT $ adaOnlyUtxoMinAdaValue <#>
          adjustAdaChangeAndSetFees unattachedTx' fees' (fees' - fees)
  where
  adjustAdaChangeAndSetFees
    :: UnattachedUnbalancedTx
    -> BigInt
    -> BigInt
    -> BigInt
    -> Either ReturnAdaChangeError UnattachedUnbalancedTx
  adjustAdaChangeAndSetFees unattachedTx' fees feesDelta changeUtxoMinValue
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
        in
          case returnAda >= changeUtxoMinValue of
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
  -> Utxos
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

-- | Given an array of transaction outputs, return the paired amount of
-- | lovelaces required by each utxo.
calculateMinUtxos
  :: Array TransactionOutput
  -> QueryM (Either UtxoMinAdaValueCalcError MinUtxos)
calculateMinUtxos = map sequence <<< traverse
  ( \txOutput ->
      utxoMinAdaValue txOutput
        <#> note UtxoMinAdaValueCalcError >>> map (Tuple txOutput)
  )

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L116
preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxos
  -> Address
  -> BigInt
  -> TxBody
  -> Either BalanceTxError TxBody
preBalanceTxBody minUtxos fees utxos ownAddr adaOnlyUtxoMinValue txBody =
  -- -- Take a single Ada only utxo collateral
  -- addTxCollaterals utxos txBody
  --   >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
  --   >>= balanceNonAdaOuts ownAddr utxos
  addLovelaces minUtxos txBody # pure
    -- Adding more inputs if required
    >>= balanceTxIns utxos fees adaOnlyUtxoMinValue
    >>= balanceNonAdaOuts ownAddr utxos

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

balanceTxIns
  :: Utxos -> BigInt -> BigInt -> TxBody -> Either BalanceTxError TxBody
balanceTxIns utxos fees changeUtxoMinValue txbody =
  balanceTxIns' utxos fees changeUtxoMinValue txbody
    # lmap BalanceTxInsError'

balanceTxIns'
  :: Utxos -> BigInt -> BigInt -> TxBody -> Either BalanceTxInsError TxBody
balanceTxIns' utxos fees changeUtxoMinValue (TxBody txBody) = do
  let
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
    minSpending = lovelaceValueOf (fees + changeUtxoMinValue) <> nonMintedValue

  -- a = spy "minSpending" minSpending

  collectTxIns txBody.inputs utxos minSpending <#>
    \txIns -> wrap txBody { inputs = Set.union txIns txBody.inputs }

--https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Getting the necessary input utxos to cover the fees for the transaction
collectTxIns
  :: Set TransactionInput
  -> Utxos
  -> Value
  -> Either BalanceTxInsError (Set TransactionInput)
collectTxIns originalTxIns utxos value = do
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

  -- Useful spies for debugging:
  -- x = spy "collectTxIns:value" value
  -- y = spy "collectTxIns:txInsValueOG" (txInsValue utxos originalTxIns)
  -- z = spy "collectTxIns:txInsValueNEW" (txInsValue utxos updatedInputs)

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

balanceNonAdaOuts
  :: Address
  -> Utxos
  -> TxBody
  -> Either BalanceTxError TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  balanceNonAdaOuts' changeAddr utxos txBody # lmap BalanceNonAdaOutsError'

-- | We need to balance non ada values as part of the prebalancer before returning
-- | excess Ada to the owner.
balanceNonAdaOuts'
  :: Address
  -> Utxos
  -> TxBody
  -> Either BalanceNonAdaOutsError TxBody
balanceNonAdaOuts' changeAddr utxos txBody'@(TxBody txBody) = do
  let
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

  if isZero nonAdaChange then pure $ wrap txBody
  -- Original code uses "isNat" because there is a guard against zero, see
  -- isPos for more detail.
  else if isPos nonAdaChange then pure $ wrap txBody { outputs = outputs }
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

getInputValue :: Utxos -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    ( Array.mapMaybe (flip Map.lookup utxos)
        <<< Array.fromFoldable
        <<< _.inputs $ txBody
    )
