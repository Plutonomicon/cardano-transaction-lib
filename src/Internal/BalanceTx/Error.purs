-- | Definitions for error types that may arise during transaction balancing,
-- | along with helpers for parsing and pretty-printing script evaluation errors
-- | that may be returned from Ogmios when calculating ex units.
module Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( BalanceInsufficientError
      , CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , InsufficientCollateralUtxos
      , CouldNotGetUtxos
      , CollateralReturnError
      , CollateralReturnMinAdaValueCalcError
      , ExUnitsEvaluationFailed
      , InsufficientUtxoBalanceToCoverAsset
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      , NumericOverflowError
      , CouldNotComputeRefScriptsFee
      )
  , Expected(Expected)
  , printTxEvaluationFailure
  , explainBalanceTxError
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Transaction.Edit (DetachedRedeemer)
import Cardano.Types
  ( Coin
  , Redeemer(Redeemer)
  , Transaction
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput))
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.UtxoMap (UtxoMap, pprintUtxoMap)
import Cardano.Types.Value (Value)
import Ctl.Internal.Helpers (bugTrackerLink, pprintTagSet)
import Ctl.Internal.QueryM.Ogmios
  ( RedeemerPointer
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , IllFormedExecutionBudget
      , NoCostModelForLanguage
      , InternalLedgerTypeConversionError
      )
  , TxEvaluationFailure(UnparsedError, AdditionalUtxoOverlap, ScriptFailures)
  ) as Ogmios
import Ctl.Internal.Types.Val (Val, pprintVal)
import Data.Array (catMaybes, filter, uncons) as Array
import Data.Bifunctor (bimap)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left, Right), either, isLeft)
import Data.Foldable (find, fold, foldMap, foldl, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (applyN)
import Data.Generic.Rep (class Generic)
import Data.Int (ceil, decimal, toNumber, toStringAs)
import Data.Lens ((^.))
import Data.Log.Tag (TagSet, tag)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String.CodePoints (length) as String
import Data.String.Common (joinWith, split) as String
import Data.String.Utils (padEnd)
import Data.UInt as UInt
import JS.BigInt as BigInt

-- | Errors conditions that may possibly arise during transaction balancing
data BalanceTxError
  = BalanceInsufficientError Val Val
  | CouldNotConvertScriptOutputToTxInput
  | CouldNotGetCollateral
  | InsufficientCollateralUtxos UtxoMap
  | CouldNotGetUtxos
  | CollateralReturnError
  | CollateralReturnMinAdaValueCalcError Coin TransactionOutput
  | ExUnitsEvaluationFailed Transaction Ogmios.TxEvaluationFailure
  | InsufficientUtxoBalanceToCoverAsset String
  | ReindexRedeemersError DetachedRedeemer
  | UtxoLookupFailedFor TransactionInput UtxoMap
  | UtxoMinAdaValueCalculationFailed
  | NumericOverflowError (Maybe Val)
  | CouldNotComputeRefScriptsFee TransactionInput

derive instance Generic BalanceTxError _

instance Show BalanceTxError where
  show e = genericShow e

explainBalanceTxError :: BalanceTxError -> String
explainBalanceTxError = case _ of
  BalanceInsufficientError expected actual ->
    "Insufficient balance. " <> prettyVal "Expected" expected
      <> ", "
      <> prettyVal "Actual" actual
  InsufficientCollateralUtxos utxos ->
    "Could not cover collateral requirements. " <>
      pprintTagSet "UTxOs for collateral selection:" (pprintUtxoMap utxos)
  CouldNotConvertScriptOutputToTxInput ->
    "Could not convert script output to transaction input"
  CouldNotGetCollateral -> "Could not get collateral from wallet"
  CouldNotGetUtxos ->
    "Could not get UTxOs from the wallet or the specified source addresses"
  CollateralReturnError ->
    "Negative totalCollateral after covering min-utxo-ada requirement."
      <> "This should be impossible: please report this as a bug to "
      <> bugTrackerLink
  CollateralReturnMinAdaValueCalcError coinsPerUtxoUnit txOut ->
    "Could not calculate minimum Ada for collateral return.\n"
      <> "Coins per UTxO unit: "
      <> show coinsPerUtxoUnit
      <> "\nTransaction output: "
      <> show txOut
  ExUnitsEvaluationFailed tx err ->
    "Script evaluation failure while trying to estimate ExUnits. Tx: "
      <> show tx
      <> ", error: "
      <> show err
  InsufficientUtxoBalanceToCoverAsset asset ->
    "Insufficient UTxO balance to cover asset named "
      <> asset
      <> "\nThis should be impossible: please report this as a bug to "
      <> bugTrackerLink
  ReindexRedeemersError uir ->
    "Could not reindex redeemer "
      <> show uir
      <> "\nThis should be impossible: please report this as a bug to "
      <> bugTrackerLink
  UtxoLookupFailedFor ti mp ->
    "Could not look up UTxO "
      <> pprintTagSet "for" (pprintTransactionInput ti)
      <> " from a "
      <> pprintTagSet "given set of UTxOs:" (pprintUtxoMap mp)
  UtxoMinAdaValueCalculationFailed ->
    "Could not calculate min ADA for UTxO"
  NumericOverflowError mbVal ->
    "Could not compute output value due to numeric overflow. Decrease the quantity of assets. "
      <> fold (prettyVal "Value:" <$> mbVal)
  CouldNotComputeRefScriptsFee input ->
    "Could not compute reference script size fees introduced in Conway. Ensure that all reference scripts are present in a UtxoMap provided to the balancer. "
      <> pprintTagSet "Missing input: " (pprintTransactionInput input)
  where
  prettyVal :: String -> Val -> String
  prettyVal str = pprintVal >>> pprintTagSet str

  pprintTransactionInput :: TransactionInput -> TagSet
  pprintTransactionInput (TransactionInput { transactionId, index }) =
    "TransactionInput" `tag`
      ( byteArrayToHex (unwrap (encodeCbor transactionId)) <> "#" <>
          show (UInt.toInt index)
      )

newtype Actual = Actual Value

derive instance Generic Actual _
derive instance Newtype Actual _

instance Show Actual where
  show = genericShow

newtype Expected = Expected Value

derive instance Generic Expected _
derive instance Newtype Expected _

instance Show Expected where
  show = genericShow

--------------------------------------------------------------------------------
-- Failure parsing for Ogmios.EvaluateTx
--------------------------------------------------------------------------------

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
  biggestPrefix = toStringAs decimal (length (Array.filter isLeft ary)) <> ". "

  width :: Prim.Int
  width = ceil (toNumber (String.length biggestPrefix) / 2.0) * 2

  numberLine :: Prim.Int -> String -> String
  numberLine i l = padEnd width (toStringAs decimal (i + 1) <> ". ") <> l

  indentLine :: String -> String
  indentLine = applyN ("  " <> _) (width / 2)

  go :: PrettyString -> Either WorkingLine FrozenLine -> PrettyString
  go b a = b <> [ bimap (numberLine $ length b) indentLine a ]

-- | Pretty print the failure response from Ogmios's EvaluateTx endpoint.
-- | Exported to allow testing, use `Test.Ogmios.Aeson.printEvaluateTxFailures`
-- | to visually verify the printing of errors without a context on fixtures.
printTxEvaluationFailure
  :: Transaction -> Ogmios.TxEvaluationFailure -> String
printTxEvaluationFailure transaction e =
  runPrettyString $ case e of
    Ogmios.UnparsedError error -> line $ "Unknown error: " <> error
    Ogmios.AdditionalUtxoOverlap utxos ->
      line $ "AdditionalUtxoOverlap: " <> show utxos
    Ogmios.ScriptFailures sf -> line "Script failures:" <> bullet
      (foldMapWithIndex printScriptFailures sf)
  where
  lookupRedeemerPointer
    :: Ogmios.RedeemerPointer -> Maybe Redeemer
  lookupRedeemerPointer ptr =
    flip find (transaction ^. _witnessSet <<< _redeemers)
      $ \(Redeemer { index, tag }) -> tag == ptr.redeemerTag && index ==
          (BigNum.fromInt $ UInt.toInt ptr.redeemerIndex)

  printRedeemerPointer :: Ogmios.RedeemerPointer -> PrettyString
  printRedeemerPointer ptr =
    line
      ( show ptr.redeemerTag <> ":" <> BigInt.toString
          (BigInt.fromInt $ UInt.toInt ptr.redeemerIndex)
      )

  -- TODO Investigate if more details can be printed, for example minting
  -- policy/minted assets
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/881
  printRedeemerDetails :: Ogmios.RedeemerPointer -> PrettyString
  printRedeemerDetails ptr =
    let
      mbRedeemerTxIn = lookupRedeemerPointer ptr
      mbData = mbRedeemerTxIn <#> \(Redeemer r) -> "Redeemer: " <> show r.data
      mbTxIn = mbRedeemerTxIn <#> \txIn -> "Input: " <> show txIn
    in
      foldMap line $ Array.catMaybes [ mbData, mbTxIn ]

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
    Ogmios.MissingRequiredScripts { resolved: mresolved, missing }
    -> line "Supplied with scripts:"
      <> bullet
        ( foldMap
            ( foldMapWithIndex
                (\ptr scr -> printRedeemer ptr <> line ("Script: " <> show scr))
            )
            mresolved
        )
      <> line "But missing required scripts:"
      <> bullet (foldMap printRedeemer missing)
    Ogmios.ValidatorFailed { error, traces } -> line error <> line "Trace:" <>
      number
        (foldMap line traces)
    Ogmios.UnknownInputReferencedByRedeemer txIn -> line
      ("Unknown input referenced by redeemer: " <> show txIn)
    Ogmios.NonScriptInputReferencedByRedeemer txIn -> line
      ( "Non-script input, or input without datum, referenced by redeemer: " <>
          show txIn
      )
    Ogmios.IllFormedExecutionBudget Nothing -> line
      ("Ill formed execution budget: Execution budget missing")
    Ogmios.IllFormedExecutionBudget (Just { memory, steps }) ->
      line "Ill formed execution budget:"
        <> bullet
          ( line ("Memory: " <> BigNum.toString memory)
              <> line ("Steps: " <> BigNum.toString steps)
          )
    Ogmios.NoCostModelForLanguage languages ->
      line "No cost model for languages:"
        <> bullet (foldMap line languages)
    Ogmios.InternalLedgerTypeConversionError error ->
      line $
        "Internal ledger type conversion error, if you ever run into this, please report the issue as you've likely discoverd a critical bug: \""
          <> error
          <> "\""

  printScriptFailures
    :: Ogmios.RedeemerPointer -> Array Ogmios.ScriptFailure -> PrettyString
  printScriptFailures ptr sfs = printRedeemer ptr <> bullet
    (foldMap printScriptFailure sfs)
