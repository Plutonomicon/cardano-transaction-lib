module Ctl.Internal.ProcessConstraints.Error where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Data.Lite (toBytes)
import Cardano.Transaction.Edit (DetachedRedeemer)
import Cardano.Types (DataHash, NativeScript)
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Cardano.Types.AssetName (AssetName, fromAssetName)
import Cardano.Types.Int as Int
import Cardano.Types.NativeScript (pprintNativeScript)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput))
import Cardano.Types.TransactionOutput
  ( TransactionOutput
  , pprintTransactionOutput
  )
import Ctl.Internal.Helpers (bugTrackerLink, pprintTagSet)
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , PosixTimeToSlotError
  , explainPosixTimeToSlotError
  )
import Data.ByteArray (byteArrayToHex)
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (tagSetTag)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt

data MkUnbalancedTxError
  = CannotFindDatum
  | CannotQueryDatum DataHash
  | CannotConvertPOSIXTimeRange POSIXTimeRange PosixTimeToSlotError
  | CannotSolveTimeConstraints POSIXTimeRange POSIXTimeRange
  | CannotGetMintingPolicyScriptIndex -- Should be impossible
  | CannotGetValidatorHashFromAddress Address -- Get `ValidatorHash` from internal `Address`
  | CannotMakeValue ScriptHash AssetName Int.Int
  | CannotWithdrawRewardsPubKey StakePubKeyHash
  | CannotWithdrawRewardsPlutusScript PlutusScript
  | CannotWithdrawRewardsNativeScript NativeScript
  | DatumNotFound DataHash
  | DatumWrongHash DataHash PlutusData
  | MintingPolicyHashNotCurrencySymbol ScriptHash
  | MintingPolicyNotFound ScriptHash
  | OwnPubKeyAndStakeKeyMissing
  | TxOutRefNotFound TransactionInput
  | TxOutRefWrongType TransactionInput
  | ValidatorHashNotFound ScriptHash
  | WrongRefScriptHash (Maybe ScriptHash) TransactionOutput
  | CannotSatisfyAny
  | ExpectedPlutusScriptGotNativeScript ScriptHash
  | CannotMintZero ScriptHash AssetName
  | NumericOverflow
  | CannotAttachRedeemer DetachedRedeemer

derive instance Generic MkUnbalancedTxError _
derive instance Eq MkUnbalancedTxError

instance Show MkUnbalancedTxError where
  show = genericShow

-- | Helper to pretty-print `MkUnbalancedTxError`s.
explainMkUnbalancedTxError :: MkUnbalancedTxError -> String
explainMkUnbalancedTxError = case _ of
  CannotFindDatum -> "Cannot find datum"
  CannotQueryDatum dh ->
    "Querying for datum by datum hash ("
      <> byteArrayToHex (unwrap $ encodeCbor dh)
      <> ") failed: no datum found"
  CannotConvertPOSIXTimeRange tr ttsErr ->
    "Cannot convert POSIX time range to slot time range.\nRange: "
      <> show tr
      <> "\nReason: "
      <> explainPosixTimeToSlotError ttsErr
  CannotSolveTimeConstraints tr tr' ->
    "Unsolvable time constraints: " <> show tr <> " and " <> show tr' <>
      " do not overlap."
  CannotGetMintingPolicyScriptIndex ->
    "Cannot get minting policy script index. This should be impossible.\n"
      <> "Please report this as a bug here: "
      <> bugTrackerLink
  CannotGetValidatorHashFromAddress addr ->
    "Cannot get a payment validator hash from address " <>
      Address.toBech32 addr
  CannotMakeValue _ tn _ ->
    "Attempted to make an amount with the ADA currency symbol, and "
      <> "non-empty token name "
      <> prettyAssetName tn
      <> ". This is not allowed, as the ADA currency symbol can only be "
      <> "combined with the empty token name."
  CannotWithdrawRewardsPubKey spkh ->
    "Cannot withdraw rewards, as pubkey "
      <> byteArrayToHex (unwrap $ encodeCbor $ unwrap spkh)
      <> " is not registered"
  CannotWithdrawRewardsPlutusScript pssv ->
    "Cannot withdraw rewards from Plutus staking script " <>
      prettyPlutusScript pssv
  CannotWithdrawRewardsNativeScript nssv ->
    pprintTagSet "Cannot withdraw rewards from native staking script "
      ("NativeScript" `tagSetTag` pprintNativeScript nssv)
  DatumNotFound hash -> "Datum with hash "
    <> byteArrayToHex (unwrap $ encodeCbor hash)
    <>
      " not found."
  DatumWrongHash dh datum -> "Datum "
    <> show datum
    <> " does not have the hash "
    <> byteArrayToHex (unwrap $ encodeCbor dh)
  MintingPolicyHashNotCurrencySymbol mph ->
    "Minting policy hash "
      <> byteArrayToHex (unwrap $ encodeCbor mph)
      <>
        " is not a CurrencySymbol. Please check the validity of the byte representation."
  MintingPolicyNotFound mp -> "Minting policy with hash "
    <> byteArrayToHex (unwrap $ encodeCbor mp)
    <> " not found in a set of minting policies"
  OwnPubKeyAndStakeKeyMissing ->
    "Could not build own address: both payment pubkey and stake pubkey are missing"
  TxOutRefNotFound ti ->
    "Could not find a reference input:\n"
      <> prettyTxIn ti
      <> "\nIt maybe have been consumed, or was never created."
  TxOutRefWrongType ti ->
    "Transaction output is missing an expected datum:\n"
      <> prettyTxIn ti
      <> "\nContext: we were trying to spend a script output."
  ValidatorHashNotFound vh -> "Cannot find validator hash: " <>
    byteArrayToHex (unwrap $ encodeCbor vh)
  WrongRefScriptHash msh tout -> case msh of
    Nothing -> pprintTagSet "Output is missing a reference script hash"
      ("TransactionOutput" `tagSetTag` pprintTransactionOutput tout)
    Just missingHash ->
      pprintTagSet
        ( "TransactionOutput is missing reference script hash "
            <> byteArrayToHex (unwrap $ encodeCbor missingHash)
        )
        ("TransactionOutput" `tagSetTag` pprintTransactionOutput tout)
  CannotSatisfyAny -> "One of the following happened:\n"
    <> "1. List of constraints is empty.\n"
    <> "2. All alternatives of a 'mustSatisfyAnyOf' have failed."
  ExpectedPlutusScriptGotNativeScript mph ->
    "Expected a Plutus script, but "
      <> byteArrayToHex (unwrap $ encodeCbor mph)
      <> " is a hash of a native script."
  CannotMintZero cs tn ->
    "Cannot mint zero of token "
      <> prettyAssetName tn
      <> " of currency "
      <> byteArrayToHex (unwrap $ encodeCbor cs)
  NumericOverflow -> "Numeric overflow"
  CannotAttachRedeemer redeemer -> do
    "Can't attach a redeemer: " <> show redeemer
      <> "\nPlease report this as a bug here: "
      <> bugTrackerLink
  where
  prettyAssetName :: AssetName -> String
  prettyAssetName = fromAssetName byteArrayToHex show

  prettyPlutusScript :: PlutusScript -> String
  prettyPlutusScript ps@(PlutusScript (_ /\ lang)) =
    show lang <> ": " <> byteArrayToHex (unwrap $ PlutusScript.getBytes ps)

  prettyTxIn :: TransactionInput -> String
  prettyTxIn (TransactionInput ti) =
    byteArrayToHex (toBytes $ unwrap ti.transactionId)
      <> "#"
      <> UInt.toString ti.index
