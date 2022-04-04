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
import FromData (class FromData)
import ToData (class ToData)
import Types.ScriptLookups
  ( MkUnbalancedTxError(..) -- A lot errors so will refrain from explicit names.
  , ScriptLookups(ScriptLookups)
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , otherDataM
  , otherScript
  , otherScriptM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  , paymentPubKeyM
  , typedValidatorLookups
  , typedValidatorLookupsM
  , unsafeOtherDataM
  , unsafePaymentPubKey
  , unspentOutputs
  , unspentOutputsM
  ) as ScriptLookups
import Types.ScriptLookups (mkUnbalancedTx) as SL
import Types.TxConstraints (TxConstraints)
import Types.TypedValidator
  ( class DatumType
  , class RedeemerType
  )
import Types.UnbalancedTransaction (UnbalancedTx)

-- | Create an `UnbalancedTx` given `ScriptLookups` and `TxConstraints`.
mkUnbalancedTx
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups.ScriptLookups a
  -> TxConstraints b b
  -> Contract r (Either ScriptLookups.MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx lookups = wrapContract <<< SL.mkUnbalancedTx lookups

-- | Same as `mkUnbalancedTx` but hushes the error.
mkUnbalancedTxM
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups.ScriptLookups a
  -> TxConstraints b b
  -> Contract r (Maybe UnbalancedTx)
mkUnbalancedTxM lookups = map hush <<< mkUnbalancedTx lookups