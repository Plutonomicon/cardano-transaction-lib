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
import Ctl.Internal.ProcessConstraints (mkUnbalancedTxImpl) as PC
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
  )
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotConvertPaymentPubKeyHash
      , CannotFindDatum
      , CannotQueryDatum
      , CannotConvertPOSIXTimeRange
      , CannotSolveTimeConstraints
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , CannotHashDatum
      , CannotHashMintingPolicy
      , CannotHashValidator
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
import Ctl.Internal.Types.Interval (explainPosixTimeToSlotError)
import Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups
  )
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either(Left, Right))
import Effect.Exception (error)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. This should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
-- |
-- | This is a 'non-throwing' variant; use this when failure is expected, and
-- | can be handled gracefully.
mkUnbalancedTx
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx = PC.mkUnbalancedTxImpl

-- | As `mkUnbalancedTx`, but 'throwing'; use this when failure is _not_
-- | expected, and graceful handling is either impossible, or wouldn't make
-- | sense.
mkUnbalancedTxE :: ScriptLookups -> TxConstraints -> Contract UnbalancedTx
mkUnbalancedTxE lookups constraints =
  mkUnbalancedTx lookups constraints >>= case _ of
    Left err -> throwError $ error $ explainMkUnbalancedTxError err
    Right res -> pure res

-- | Helper to pretty-print `MkUnbalancedTxError`s.
explainMkUnbalancedTxError :: MkUnbalancedTxError -> String
explainMkUnbalancedTxError = case _ of
  X.CannotConvertPaymentPubKeyHash ppkh ->
    "Cannot convert payment pubkey hash: " <> show ppkh
  X.CannotFindDatum -> "Cannot find datum."
  X.CannotQueryDatum dh -> "Cannot query datum: " <> show dh
  X.CannotConvertPOSIXTimeRange tr ttsErr ->
    "Cannot convert POSIX time range: " <> show tr <> "\nReason: " <>
      explainPosixTimeToSlotError ttsErr
  X.CannotSolveTimeConstraints tr tr' ->
    "Unsolvable time constraints: " <> show tr <> " and " <> show tr' <>
      " do not overlap."
  X.CannotGetMintingPolicyScriptIndex ->
    "Cannot get minting policy script index. This should be impossible."
  X.CannotGetValidatorHashFromAddress addr ->
    "Cannot get a validator hash from address " <> show addr
  X.CannotHashDatum datum ->
    "Cannot hash datum: " <> show datum
  X.CannotHashMintingPolicy mp ->
    "Cannot hash minting policy: " <> show mp
  X.CannotHashValidator validator ->
    "Cannot hash validator: " <> show validator
  X.CannotMakeValue cs tn amount ->
    "Cannot make " <> show amount <> " of token " <> show tn <> " of currency "
      <> show cs
  X.CannotWithdrawRewardsPubKey spkh ->
    "Cannot withdraw rewards from staking pubkey " <> show spkh
  X.CannotWithdrawRewardsPlutusScript pssv ->
    "Cannot withdraw rewards from Plutus staking script " <> show pssv
  X.CannotWithdrawRewardsNativeScript nssv ->
    "Cannot withdraw rewards from native staking script " <> show nssv
  X.DatumNotFound hash -> "Datum with hash " <> show hash <> " not found."
  X.DatumWrongHash hash datum -> "Datum " <> show datum
    <> " does not have the hash "
    <> show hash
  X.MintingPolicyHashNotCurrencySymbol mph ->
    "Minting policy hash " <> show mph <>
      " is not the hash of a CurrencySymbol."
  X.MintingPolicyNotFound mp -> "Minting policy not found: " <> show mp
  X.ModifyTx modifyTxErr -> explainModifyTxError modifyTxErr
  X.OwnPubKeyAndStakeKeyMissing -> "Own pubkey and staking key is missing."
  X.TxOutRefNotFound ti -> "Cannot find transaction input: " <> show ti
  X.TxOutRefWrongType ti ->
    "Transaction input is missing an expected datum: " <> show ti
  X.ValidatorHashNotFound vh -> "Cannot find validator hash: " <> show vh
  X.WrongRefScriptHash msh ->
    "Output is missing a reference script hash: " <> show msh
  X.CannotSatisfyAny -> "List of constraints is empty."
  X.ExpectedPlutusScriptGotNativeScript mph ->
    "Expected a Plutus script, but " <> show mph <> "is a native script."
  X.CannotMintZero cs tn -> "Cannot mint zero of token " <> show tn
    <> " of currency "
    <> show cs
