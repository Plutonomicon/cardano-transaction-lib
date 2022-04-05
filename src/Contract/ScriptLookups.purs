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
  , otherData
  ) where

import Prelude
import Contract.Monad (Contract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import FromData (class FromData)
import ToData (class ToData)
import Types.Datum (Datum)
import Types.ScriptLookups
  ( MkUnbalancedTxError(..) -- A lot errors so will refrain from explicit names.
  , ScriptLookups(ScriptLookups)
  , UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , otherScript
  , otherScriptM
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
import Types.ScriptLookups (otherData, mkUnbalancedTx) as SL
import Types.TxConstraints (TxConstraints)
import Types.TypedValidator
  ( class DatumType
  , class RedeemerType
  )

-- | Create an `UnattachedUnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. You will probably want to use this version as it returns
-- | datums and redeemers that require attaching (and maybe reindexing) in
-- | a separate call. In particular, this should be called in conjuction with
-- | `balanceAndSignTx`.
mkUnbalancedTx
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups.ScriptLookups a
  -> TxConstraints b b
  -> Contract
       ( Either
           ScriptLookups.MkUnbalancedTxError
           ScriptLookups.UnattachedUnbalancedTx
       )
mkUnbalancedTx lookups = wrap <<< SL.mkUnbalancedTx lookups

-- | Same as `mkUnbalancedTx` but hushes the error.
mkUnbalancedTxM
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups.ScriptLookups a
  -> TxConstraints b b
  -> Contract (Maybe ScriptLookups.UnattachedUnbalancedTx)
mkUnbalancedTxM lookups = map hush <<< mkUnbalancedTx lookups

otherData
  :: forall (a :: Type)
   . Datum
  -> Contract (Maybe (ScriptLookups.ScriptLookups a))
otherData = wrap <<< SL.otherData