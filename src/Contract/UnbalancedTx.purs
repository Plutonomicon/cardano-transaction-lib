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
import Ctl.Internal.Cardano.Types.Value (pprintValue)
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
import Ctl.Internal.Transaction (explainModifyTxError)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.Interval (explainPosixTimeToSlotError)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PlutusData (pprintPlutusData)
import Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups
  )
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either(Left, Right))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
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
  CannotQueryDatum dh ->
    "Querying for datum by datum hash ("
      <> show dh
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
    "Cannot get a payment validator hash from address " <> show addr
  CannotMakeValue _ tn _ ->
    "Attempted to make an amount with the ADA currency symbol, and non-empty token name "
      <> show tn
      <>
        ". This is not allowed, as the ADA currency symbol can only be combined with the empty token name."
  CannotWithdrawRewardsPubKey spkh ->
    "Cannot withdraw rewards, as pubkey " <> show spkh <> " is not registered"
  CannotWithdrawRewardsPlutusScript pssv ->
    "Cannot withdraw rewards from Plutus staking script " <> show pssv
  CannotWithdrawRewardsNativeScript nssv ->
    "Cannot withdraw rewards from native staking script " <> show nssv
  DatumNotFound hash -> "Datum with hash " <> show hash <> " not found."
  DatumWrongHash hash datum -> "Datum " <> show datum
    <> " does not have the hash "
    <> show hash
  MintingPolicyHashNotCurrencySymbol mph ->
    "Minting policy hash " <> show mph <>
      " is not a CurrencySymbol. Please check the validity of the byte representation."
  MintingPolicyNotFound mp -> "Minting policy "
    <> show mp
    <> " not found in a set of minting policies"
  ModifyTx modifyTxErr -> explainModifyTxError modifyTxErr
  OwnPubKeyAndStakeKeyMissing ->
    "Could not build own address: both payment pubkey and stake pubkey are missing"
  TxOutRefNotFound ti ->
    "Could not find a reference input ("
      <> show ti
      <>
        "). It maybe have been consumed, or was never created."
  TxOutRefWrongType ti ->
    "Transaction output is missing an expected datum: "
      <> show ti
      <>
        "\nContext: we were trying to spend a script output."
  ValidatorHashNotFound vh -> "Cannot find validator hash: " <> show vh
  WrongRefScriptHash msh tout ->
    "Output is missing a reference script hash: "
      <> show msh
      <> "\nOutput: "
      <>
        prettyOutput tout
  CannotSatisfyAny -> "One of the following happened:\n"
    <> "1. List of constraints is empty.\n"
    <>
      "2. All alternatives of a 'mustSatisfyAnyOf' have failed."
  ExpectedPlutusScriptGotNativeScript mph ->
    "Expected a Plutus script, but " <> show mph <> "is a native script."
  CannotMintZero cs tn -> "Cannot mint zero of token " <> show tn
    <> " of currency "
    <> show cs
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

-- Helpers

bugTrackerLink :: String
bugTrackerLink =
  "https://github.com/Plutonomicon/cardano-transaction-lib/issues"
