-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
-- |
-- | The lookup functions come in pairs. If the function cannot fail, there
-- | is another version contained in a `Maybe` context (that also does not fail).
-- | This is to aid users who wish to utilise the underlying `ScriptLookups`
-- | `Monoid` for `foldMap` etc.
-- |
-- | Otherwise, there are lookups that may fail with `Maybe` (because of
-- | hashing) and an unsafe counterpart via `fromJust`.
module Contract.ScriptLookups
  ( mkUnbalancedTx
  , mkUnbalancedTxM
  , module ScriptLookups
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.IsData (class IsData)
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx(UnbalancedTx)) as X
import Ctl.Internal.Types.ScriptLookups
  ( MkUnbalancedTxError
      ( TypeCheckFailed
      , ModifyTx
      , TxOutRefNotFound
      , TxOutRefWrongType
      , DatumNotFound
      , MintingPolicyNotFound
      , MintingPolicyHashNotCurrencySymbol
      , CannotMakeValue
      , ValidatorHashNotFound
      , OwnPubKeyAndStakeKeyMissing
      , TypedValidatorMissing
      , DatumWrongHash
      , CannotQueryDatum
      , CannotHashDatum
      , CannotConvertPOSIXTimeRange
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , TypedTxOutHasNoDatumHash
      , CannotHashMintingPolicy
      , CannotHashValidator
      , CannotConvertPaymentPubKeyHash
      , CannotSatisfyAny
      )
  , ScriptLookups(ScriptLookups)
  , datum
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  , typedValidatorLookups
  , typedValidatorLookupsM
  -- , paymentPubKeyM
  , unspentOutputs
  , unspentOutputsM
  -- , unsafePaymentPubKey
  , validator
  , validatorM
  ) as ScriptLookups
import Ctl.Internal.Types.ScriptLookups (mkUnbalancedTx) as SL
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Ctl.Internal.Types.TypedValidator (class ValidatorTypes)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. You will probably want to use this version as it returns
-- | datums and redeemers that require attaching (and maybe reindexing) in
-- | a separate call. In particular, this should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
mkUnbalancedTx
  :: forall (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract
       ( Either
           ScriptLookups.MkUnbalancedTxError
           UnbalancedTx
       )
mkUnbalancedTx = SL.mkUnbalancedTx

-- | Same as `mkUnbalancedTx` but hushes the error.
mkUnbalancedTxM
  :: forall (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract (Maybe UnbalancedTx)
mkUnbalancedTxM lookups = map hush <<< mkUnbalancedTx lookups
