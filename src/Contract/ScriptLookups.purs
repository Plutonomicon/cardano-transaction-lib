-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
module Contract.ScriptLookups
  ( mkUnbalancedTx
  , module X
  ) where

import Contract.Monad (Contract)
import Ctl.Internal.ProcessConstraints (mkUnbalancedTxImpl) as PC
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
  )
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotFindDatum
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
      , ValidatorHashNotFound
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
import Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups(ScriptLookups)
  , datum
  , mintingPolicy
  , mintingPolicyM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  -- , paymentPubKeyM
  , unspentOutputs
  , unspentOutputsM
  -- , unsafePaymentPubKey
  , validator
  , validatorM
  ) as X
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. You will probably want to use this version as it returns
-- | datums and redeemers that require attaching (and maybe reindexing) in
-- | a separate call. In particular, this should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
mkUnbalancedTx
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx = PC.mkUnbalancedTxImpl
