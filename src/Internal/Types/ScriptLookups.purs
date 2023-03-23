module Ctl.Internal.Types.ScriptLookups
  ( ScriptLookups(ScriptLookups)
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , datum
  , validator
  , validatorM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  , typedValidatorLookups
  , typedValidatorLookupsM
  , unspentOutputs
  , unspentOutputsM
  ) where

import Prelude hiding (join)

import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.Helpers ((<\>))
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutputWithRefScript) as Plutus
import Ctl.Internal.Types.Any (Any)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)
import Ctl.Internal.Types.Scripts (MintingPolicy, Validator)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.TypedValidator (TypedValidator(TypedValidator))
import Ctl.Internal.Types.TypedValidator (generalise) as TV
import Ctl.Internal.Types.UnbalancedTransaction (PaymentPubKey)
import Data.Array (singleton, union) as Array
import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty, singleton, union)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over, wrap)
import Data.Show.Generic (genericShow)

-- Taken mainly from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- It should be noted that `ScriptOutput` came later and this was already apart
-- of our codebase so I had to mix & match Plutus revs.

--------------------------------------------------------------------------------
-- ScriptLookups type
--------------------------------------------------------------------------------
-- We write `mps` and `scripts` as an `Array` instead of `Map`, meaning
-- our lookup helpers aren't required to hash (`mintingPolicy`, `validator`)
-- and therefore not lifted to `QueryM`. The downside is the lookups contain
-- less information. All hashing is done inside `ConstraintsM`, see
-- `processLookupsAndConstraints`.
-- The lookups uses the Plutus type `TransactionOutput` and does internal
-- conversions to the Serialization/Cardano to append to the `TxBody` as needed.
newtype ScriptLookups (a :: Type) = ScriptLookups
  { mps ::
      Array MintingPolicy -- Minting policies that the script interacts with
  , txOutputs ::
      Map TransactionInput Plutus.TransactionOutputWithRefScript -- Unspent outputs that the script may want to spend
  , scripts ::
      Array Validator -- Script validators
  , datums :: Map DataHash Datum --  Datums that we might need
  -- FIXME there's currently no way to set this field
  -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/569
  , paymentPubKeyHashes ::
      Map PaymentPubKeyHash PaymentPubKey -- Public keys that we might need
  , typedValidator ::
      Maybe (TypedValidator a) -- The script instance with the typed validator hash & actual compiled program
  , ownPaymentPubKeyHash ::
      Maybe PaymentPubKeyHash -- The contract's payment public key hash, used for depositing tokens etc.
  , ownStakePubKeyHash ::
      Maybe StakePubKeyHash -- The contract's stake public key hash (optional)
  }

derive instance Generic (ScriptLookups a) _
derive instance Newtype (ScriptLookups a) _
derive newtype instance Eq (ScriptLookups a)

instance Show (ScriptLookups a) where
  show = genericShow

generalise :: forall (a :: Type). ScriptLookups a -> ScriptLookups Any
generalise (ScriptLookups sl) =
  let
    tv = TV.generalise <$> sl.typedValidator
  in
    wrap sl { typedValidator = tv }

-- Using `Data.Map.union`, we can replicate left-biased <> from Data.Map used
-- in Plutus (*not* Plutus' internal Map that uses something like unionWith (<>))
instance Semigroup (ScriptLookups a) where
  append (ScriptLookups l) (ScriptLookups r) =
    ScriptLookups
      { mps: l.mps `Array.union` r.mps
      , txOutputs: l.txOutputs `union` r.txOutputs
      , scripts: l.scripts `Array.union` r.scripts
      , datums: l.datums `union` r.datums
      , paymentPubKeyHashes: l.paymentPubKeyHashes `union` r.paymentPubKeyHashes
      -- 'First' to match the semigroup instance of Map (left-biased)
      , typedValidator: l.typedValidator <\> r.typedValidator
      , ownPaymentPubKeyHash: l.ownPaymentPubKeyHash <\> r.ownPaymentPubKeyHash
      , ownStakePubKeyHash: l.ownStakePubKeyHash <\> r.ownStakePubKeyHash
      }

instance Monoid (ScriptLookups a) where
  mempty = ScriptLookups
    { mps: mempty
    , txOutputs: empty
    , scripts: mempty
    , datums: empty
    , paymentPubKeyHashes: empty
    , typedValidator: Nothing
    , ownPaymentPubKeyHash: Nothing
    , ownStakePubKeyHash: Nothing
    }

--------------------------------------------------------------------------------
-- Create ScriptLookups helpers
--------------------------------------------------------------------------------
-- | The lookup functions come in pairs with the exception of `datum`.
-- | If the function cannot fail, there is another version contained in a `Maybe`
-- | context (that also does not fail).
-- | This is to aid users who wish to utilise the underlying `ScriptLookups`
-- | `Monoid` for `foldMap` etc.
-- |
-- | Otherwise, there are lookups that may fail with `Maybe` (because of
-- | hashing) and an unsafe counterpart via `fromJust`.

-- | A script lookups value with a script instance. For convenience this also
-- | includes the minting policy script that forwards all checks to the
-- | instance's validator.
typedValidatorLookups :: forall (a :: Type). TypedValidator a -> ScriptLookups a
typedValidatorLookups tv@(TypedValidator inst) =
  over ScriptLookups
    _
      { mps = Array.singleton inst.forwardingMPS
      , typedValidator = Just tv
      }
    mempty

-- | Same as `typedValidatorLookups` but in `Maybe` context for convenience.
-- | This should not fail.
typedValidatorLookupsM
  :: forall (a :: Type). TypedValidator a -> Maybe (ScriptLookups a)
typedValidatorLookupsM = pure <<< typedValidatorLookups

-- | A script lookups value that uses the map of unspent outputs to resolve
-- | input constraints.
unspentOutputs
  :: forall (a :: Type)
   . Map TransactionInput Plutus.TransactionOutputWithRefScript
  -> ScriptLookups a
unspentOutputs mp = over ScriptLookups _ { txOutputs = mp } mempty

-- | Same as `unspentOutputs` but in `Maybe` context for convenience.
-- | This should not fail.
unspentOutputsM
  :: forall (a :: Type)
   . Map TransactionInput Plutus.TransactionOutputWithRefScript
  -> Maybe (ScriptLookups a)
unspentOutputsM = pure <<< unspentOutputs

-- | A script lookups value with a minting policy script.
mintingPolicy :: forall (a :: Type). MintingPolicy -> ScriptLookups a
mintingPolicy pl = over ScriptLookups _ { mps = Array.singleton pl } mempty

-- | Same as `mintingPolicy` but in `Maybe` context for convenience. This
-- | should not fail.
mintingPolicyM :: forall (a :: Type). MintingPolicy -> Maybe (ScriptLookups a)
mintingPolicyM = pure <<< mintingPolicy

-- | A script lookups value with a validator script.
validator :: forall (a :: Type). Validator -> ScriptLookups a
validator vl =
  over ScriptLookups _ { scripts = Array.singleton vl } mempty

-- | Same as `validator` but in `Maybe` context for convenience. This
-- | should not fail.
validatorM :: forall (a :: Type). Validator -> Maybe (ScriptLookups a)
validatorM = pure <<< validator

-- | A script lookups value with a datum.
datum :: forall (a :: Type). Datum -> ScriptLookups a
datum dt =
  over ScriptLookups _ { datums = singleton (Hashing.datumHash dt) dt } mempty

-- | Add your own `PaymentPubKeyHash` to the lookup.
ownPaymentPubKeyHash :: forall (a :: Type). PaymentPubKeyHash -> ScriptLookups a
ownPaymentPubKeyHash pkh =
  over ScriptLookups _ { ownPaymentPubKeyHash = Just pkh } mempty

-- | Same as `ownPaymentPubKeyHash` but in `Maybe` context for convenience. This
-- | should not fail.
ownPaymentPubKeyHashM
  :: forall (a :: Type). PaymentPubKeyHash -> Maybe (ScriptLookups a)
ownPaymentPubKeyHashM = pure <<< ownPaymentPubKeyHash

-- | Add your own `StakePubKeyHash` to the lookup.
ownStakePubKeyHash :: forall (a :: Type). StakePubKeyHash -> ScriptLookups a
ownStakePubKeyHash skh =
  over ScriptLookups _ { ownStakePubKeyHash = Just skh } mempty

-- | Same as `ownStakePubKeyHash` but in `Maybe` context for convenience. This
-- | should not fail.
ownStakePubKeyHashM
  :: forall (a :: Type). StakePubKeyHash -> Maybe (ScriptLookups a)
ownStakePubKeyHashM = pure <<< ownStakePubKeyHash
