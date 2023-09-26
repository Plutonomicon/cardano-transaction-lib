-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
module Contract.UnbalancedTx
  ( mkUnbalancedTx
  , mkUnbalancedTxM
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
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
      , CannotSatisfyAny
      , ExpectedPlutusScriptGotNativeScript
      , CannotMintZero
      )
  ) as X
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx(UnbalancedTx)) as X
import Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups
  )
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. This should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
mkUnbalancedTx
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx = PC.mkUnbalancedTxImpl

-- | Same as `mkUnbalancedTx` but hushes the error.
-- TODO: remove, reason: it's trivial
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1047
mkUnbalancedTxM
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Maybe UnbalancedTx)
mkUnbalancedTxM lookups = map hush <<< mkUnbalancedTx lookups
