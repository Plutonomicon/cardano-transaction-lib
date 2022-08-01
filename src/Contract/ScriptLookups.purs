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
  ) where

import Prelude
import Contract.Monad (Contract, wrapContract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import IsData (class IsData)
import Types.ScriptLookups
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
      , MkTypedTxOutFailed
      , TypedTxOutHasNoDatumHash
      , CannotHashMintingPolicy
      , CannotHashValidator
      , CannotConvertPaymentPubKeyHash
      , CannotSatisfyAny
      )
  , ScriptLookups(ScriptLookups)
  , UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  , datum
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , validator
  , validatorM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  -- , paymentPubKeyM
  , typedValidatorLookups
  , typedValidatorLookupsM
  -- , unsafePaymentPubKey
  , unspentOutputs
  , unspentOutputsM
  ) as ScriptLookups
import Types.ScriptLookups (mkUnbalancedTx) as SL
import Types.TxConstraints (TxConstraints)
import Types.TypedValidator (class ValidatorTypes)

-- | Create an `UnattachedUnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. You will probably want to use this version as it returns
-- | datums and redeemers that require attaching (and maybe reindexing) in
-- | a separate call. In particular, this should be called in conjuction with
-- | `balanceAndSignTx`.
mkUnbalancedTx
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract r
       ( Either
           ScriptLookups.MkUnbalancedTxError
           ScriptLookups.UnattachedUnbalancedTx
       )
mkUnbalancedTx lookups = wrapContract <<< SL.mkUnbalancedTx lookups

-- | Same as `mkUnbalancedTx` but hushes the error.
mkUnbalancedTxM
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract r (Maybe ScriptLookups.UnattachedUnbalancedTx)
mkUnbalancedTxM lookups = map hush <<< mkUnbalancedTx lookups
