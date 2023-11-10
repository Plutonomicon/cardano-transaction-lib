-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
module Contract.UnbalancedTx
  ( mkUnbalancedTx
  , mkUnbalancedTxE
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.ProcessConstraints (mkUnbalancedTxImpl) as PC
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
  , explainMkUnbalancedTxError
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
  , explainMkUnbalancedTxError
  ) as X
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx(UnbalancedTx)) as X
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either(Left, Right))
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
