-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
module Contract.UnbalancedTx
  ( mkUnbalancedTx
  , mkUnbalancedTxE
  , explainMkUnbalancedTxError
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Ctl.Internal.Cardano.Types.Value
  ( getCurrencySymbol
  , pprintValue
  )
import Ctl.Internal.Plutus.Conversion.Value (fromPlutusValue)
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  )
import Ctl.Internal.ProcessConstraints (mkUnbalancedTxImpl) as PC
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotFindDatum
      , CannotQueryDatum
      , CannotConvertPOSIXTimeRange
      , CannotSolveTimeConstraints
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , CannotMakeValue
      , CannotWithdrawRewardsPubKey
      , CannotWithdrawRewardsPlutusScript
      , CannotWithdrawRewardsNativeScript
      , DatumNotFound
      , DatumWrongHash
      , MintingPolicyHashNotCurrencySymbol
      , MintingPolicyNotFound
      , ModifyTx
      , OwnPubKeyAndStakeKeyMissing
      , TxOutRefNotFound
      , TxOutRefWrongType
      , WrongRefScriptHash
      , ValidatorHashNotFound
      , CannotSatisfyAny
      , ExpectedPlutusScriptGotNativeScript
      , CannotMintZero
      )
  )
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotFindDatum
      , CannotQueryDatum
      , CannotConvertPOSIXTimeRange
      , CannotSolveTimeConstraints
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , CannotMakeValue
      , CannotWithdrawRewardsPubKey
      , CannotWithdrawRewardsPlutusScript
      , CannotWithdrawRewardsNativeScript
      , DatumNotFound
      , DatumWrongHash
      , MintingPolicyHashNotCurrencySymbol
      , MintingPolicyNotFound
      , ModifyTx
      , OwnPubKeyAndStakeKeyMissing
      , TxOutRefNotFound
      , TxOutRefWrongType
      , WrongRefScriptHash
      , ValidatorHashNotFound
      , CannotSatisfyAny
      , ExpectedPlutusScriptGotNativeScript
      , CannotMintZero
      )
  ) as X
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx(UnbalancedTx)) as X
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashToBytes
  , scriptHashToBytes
  )
import Ctl.Internal.Transaction (explainModifyTxError)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.Interval (explainPosixTimeToSlotError)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PlutusData (pprintPlutusData)
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups
  )
import Ctl.Internal.Types.Scripts (PlutusScript(PlutusScript))
import Ctl.Internal.Types.TokenName (TokenName, fromTokenName)
import Ctl.Internal.Types.Transaction
  ( DataHash(DataHash)
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either(Left, Right))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. This should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
-- |
-- | This is a 'non-throwing' variant; if you need the 'throwing' variant, use
-- | `mkUnbalancedTx` instead.
mkUnbalancedTxE
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTxE = PC.mkUnbalancedTxImpl

-- | As `mkUnbalancedTxE`, but 'throwing'.
mkUnbalancedTx :: ScriptLookups -> TxConstraints -> Contract UnbalancedTx
mkUnbalancedTx lookups constraints =
  mkUnbalancedTxE lookups constraints >>= case _ of
    Left err -> throwError $ error $ explainMkUnbalancedTxError err
    Right res -> pure res

-- | Helper to pretty-print `MkUnbalancedTxError`s.
explainMkUnbalancedTxError :: MkUnbalancedTxError -> String
explainMkUnbalancedTxError = case _ of
  CannotFindDatum -> "Cannot find datum"
  CannotQueryDatum (DataHash dh) ->
    "Querying for datum by datum hash ("
      <> byteArrayToHex dh
      <>
        ") failed: no datum found"
  CannotConvertPOSIXTimeRange tr ttsErr ->
    "Cannot convert POSIX time range to slot time range.\nRange: "
      <> show tr
      <> "\nReason: "
      <>
        explainPosixTimeToSlotError ttsErr
  CannotSolveTimeConstraints tr tr' ->
    "Unsolvable time constraints: " <> show tr <> " and " <> show tr' <>
      " do not overlap."
  CannotGetMintingPolicyScriptIndex ->
    "Cannot get minting policy script index. This should be impossible.\n"
      <> "Please report this as a bug here: "
      <> bugTrackerLink
  CannotGetValidatorHashFromAddress addr ->
    "Cannot get a payment validator hash from address " <>
      addressBech32 addr
  CannotMakeValue _ tn _ ->
    "Attempted to make an amount with the ADA currency symbol, and non-empty token name "
      <> prettyTokenName tn
      <>
        ". This is not allowed, as the ADA currency symbol can only be combined with the empty token name."
  CannotWithdrawRewardsPubKey spkh ->
    "Cannot withdraw rewards, as pubkey "
      <> rawBytesToHex (ed25519KeyHashToBytes $ unwrap $ unwrap spkh)
      <>
        " is not registered"
  CannotWithdrawRewardsPlutusScript pssv ->
    "Cannot withdraw rewards from Plutus staking script " <>
      prettyPlutusScript (unwrap pssv)
  CannotWithdrawRewardsNativeScript nssv ->
    "Cannot withdraw rewards from native staking script " <>
      prettyNativeScript 0 (unwrap nssv)
  DatumNotFound (DataHash hash) -> "Datum with hash " <> byteArrayToHex hash <>
    " not found."
  DatumWrongHash (DataHash dh) datum -> "Datum "
    <> show datum
    <> " does not have the hash "
    <> byteArrayToHex dh
  MintingPolicyHashNotCurrencySymbol mph ->
    "Minting policy hash "
      <> rawBytesToHex (scriptHashToBytes $ unwrap mph)
      <>
        " is not a CurrencySymbol. Please check the validity of the byte representation."
  MintingPolicyNotFound mp -> "Minting policy with hash "
    <> rawBytesToHex (scriptHashToBytes $ unwrap mp)
    <> " not found in a set of minting policies"
  ModifyTx modifyTxErr -> explainModifyTxError modifyTxErr
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
    rawBytesToHex (scriptHashToBytes $ unwrap vh)
  WrongRefScriptHash msh tout -> case msh of
    Nothing -> "Output is missing a reference script hash.\nOutput:\n" <>
      prettyOutput tout
    Just missingHash -> "Output is missing reference script hash "
      <> rawBytesToHex (scriptHashToBytes missingHash)
      <> ".\nOutput:\n"
      <>
        prettyOutput tout
  CannotSatisfyAny -> "One of the following happened:\n"
    <> "1. List of constraints is empty.\n"
    <>
      "2. All alternatives of a 'mustSatisfyAnyOf' have failed."
  ExpectedPlutusScriptGotNativeScript mph ->
    "Expected a Plutus script, but "
      <> rawBytesToHex (scriptHashToBytes $ unwrap mph)
      <>
        " is a native script."
  CannotMintZero cs tn ->
    "Cannot mint zero of token "
      <> prettyTokenName tn
      <> " of currency "
      <>
        byteArrayToHex (getCurrencySymbol cs)
  where
  prettyOutput :: TransactionOutput -> String
  prettyOutput (TransactionOutput { address, amount, datum, referenceScript }) =
    let
      datumTagSets = map TagSet.fromArray $ case datum of
        NoOutputDatum -> []
        OutputDatumHash datumHash ->
          [ [ "datumHash" `TagSet.tag` byteArrayToHex
                (unwrap datumHash)
            ]
          ]
        OutputDatum plutusData ->
          [ [ "datum" `TagSet.tagSetTag`
                pprintPlutusData (unwrap plutusData)
            ]
          ]
      scriptRefTagSets = case referenceScript of
        Nothing -> []
        Just ref -> [ "Script Reference" `TagSet.tag` show ref ]
      outputTagSet =
        [ "amount" `TagSet.tagSetTag` pprintValue (fromPlutusValue amount)
        , "address" `TagSet.tag` (show address)
        ] <> datumTagSets <> scriptRefTagSets
    in
      foldMapWithIndex prettyEntry $ TagSet.fromArray outputTagSet

  prettyEntry :: String -> TagSet.Tag -> String
  prettyEntry k v = (k <> ": ")
    <>
      ( case v of
          TagSet.StringTag s -> s
          TagSet.NumberTag n -> show n
          TagSet.IntTag i -> show i
          TagSet.BooleanTag b -> show b
          TagSet.JSDateTag date -> show date
          TagSet.TagSetTag ts -> foldMapWithIndex prettyEntry ts
      )
    <> "\n"

  prettyTokenName :: TokenName -> String
  prettyTokenName = fromTokenName byteArrayToHex identity

  prettyPlutusScript :: PlutusScript -> String
  prettyPlutusScript (PlutusScript (code /\ lang)) =
    show lang <> ": " <> byteArrayToHex code

  prettyTxIn :: TransactionInput -> String
  prettyTxIn (TransactionInput ti) =
    "Id: "
      <> byteArrayToHex (unwrap ti.transactionId)
      <> "\nIndex: "
      <>
        show ti.index

  prettyNativeScript :: Int -> NativeScript -> String
  prettyNativeScript indent script =
    let
      newIndent = indent + 1
    in
      case script of
        ScriptPubkey kh -> rawBytesToHex $ ed25519KeyHashToBytes kh
        ScriptAll scripts -> "All of:\n" <>
          joinWithIndentNewline newIndent scripts
        ScriptAny scripts -> "At least one of:\n" <>
          joinWithIndentNewline newIndent scripts
        ScriptNOfK n scripts -> "At least " <> show n <> " of:\n" <>
          joinWithIndentNewline newIndent scripts
        TimelockStart slot -> "Timelock start for slot " <> show (unwrap slot)
        TimelockExpiry slot -> "Timelock expiry for slot " <> show (unwrap slot)

  joinWithIndentNewline :: Int -> Array NativeScript -> String
  joinWithIndentNewline indent = map (prettyNativeScript indent) >>>
    String.joinWith ("\n" <> spaces (2 * indent))

  spaces :: Int -> String
  spaces n
    | n <= 0 = ""
    | otherwise = " " <> spaces (n - 1)

-- Helpers

bugTrackerLink :: String
bugTrackerLink =
  "https://github.com/Plutonomicon/cardano-transaction-lib/issues"
