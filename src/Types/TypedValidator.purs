module Types.TypedValidator
  ( TypedValidator(TypedValidator)
  , ValidatorType
  , WrappedValidatorType
  , class DatumType
  , class RedeemerType
  , class ValidatorTypes
  , forwardingMintingPolicy
  , generalise
  , typedValidatorHash
  , typedValidatorScript
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , (.:)
  )
import Cardano.Types.Transaction (Transaction)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Types.Any (Any)
import Types.PlutusData (PlutusData)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , Validator
  , ValidatorHash
  )

-- We don't need c, d because a determines the same type b for `DatumType` and
-- `RedeemerType`. Plutus uses associated typed families, but we don't have
-- such capabilities and therefore use functional dependencies instead.
-- An alternative formulation could be:
-- `ValidatorTypes (a :: Type) (b :: Type) | a -> b`
-- then
-- class ValidatorType a b <= DatumType (a :: Type) (b :: Type) | a -> b
-- class ValidatorType a b <= RedeemerType (a :: Type) (b :: Type) | a -> b
-- separately. We'll use the below implementation for now and determine whether
-- it suffices.
-- | A typeclass that associates a type standing for a connection type with two
-- | types, the type of the redeemer and the data script for that connection type.
class ValidatorTypes :: Type -> Type -> Type -> Constraint
class
  ( DatumType v d
  , RedeemerType v r
  ) <=
  ValidatorTypes (v :: Type) (d :: Type) (r :: Type)

instance (DatumType v d, RedeemerType v r) => ValidatorTypes v d r

-- | The type of the data of this connection type.
class DatumType (v :: Type) (d :: Type) | v -> d

instance DatumType Void Void

instance DatumType Any PlutusData

instance DatumType PlutusData Unit

-- | The type of the redeemers of this connection type.
class RedeemerType (v :: Type) (r :: Type) | v -> r

instance RedeemerType Void Void

instance RedeemerType Any PlutusData

instance RedeemerType PlutusData Unit

-- Replace `ScriptContext` by `Transaction` which contains all the scripts
-- anyway:
-- | The type of validators for the given connection type.
type ValidatorType (v :: Type) (d :: Type) (r :: Type) =
  DatumType v d => RedeemerType v r => d -> r -> Transaction -> Boolean

type WrappedValidatorType =
  PlutusData -> PlutusData -> PlutusData -> Effect Unit

-- Would require `ToData/FromData` for `Transaction`:
-- wrapValidator
--   :: forall (d :: Type) (r :: Type)
--    . FromData d
--   => FromData r
--   => (d -> r -> Transaction -> Boolean)
--   -> WrappedValidatorType
-- wrapValidator f d r p =
--   map (const unit) $ fromJustEff "Failed wrapValidator" $
--     f <$> fromData d <*> fromData r <*> fromData p

-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- | A typed validator script with its `ValidatorScript` and `Address`.
newtype TypedValidator (a :: Type) = TypedValidator
  { validator :: Validator
  , validatorHash :: ValidatorHash
  , forwardingMPS :: MintingPolicy
  , forwardingMPSHash :: MintingPolicyHash -- Can maybe remove as unused in lookups.
  -- The hash of the minting policy that checks whether the validator
  -- is run in this transaction
  }

derive instance Generic (TypedValidator a) _
derive instance Newtype (TypedValidator a) _
derive newtype instance Eq (TypedValidator a)

instance Show (TypedValidator a) where
  show = genericShow

instance DecodeAeson (TypedValidator a) where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected Object")
    ( \o -> do
        validator <- o .: "validator"
        validatorHash <- o .: "validatorHash"
        forwardingMPS <- o .: "forwardingMPS"
        forwardingMPSHash <- o .: "forwardingMPSHash"
        pure $ TypedValidator
          { validator, validatorHash, forwardingMPS, forwardingMPSHash }
    )

-- Not sure how necessary this is:
-- | Generalise the typed validator to one that works with the `PlutusData` type.
generalise :: forall (a :: Type). TypedValidator a -> TypedValidator Any
generalise
  ( TypedValidator
      { validator, validatorHash: vh, forwardingMPS, forwardingMPSHash }
  ) =
  -- we can do this safely because the on-chain validators are untyped, so they always
  -- take 'PlutusData' arguments. The validator script stays the same, so the conversion
  -- from 'PlutusData' to 'a' still takes place, even if it's not reflected in the type
  -- signature anymore.
  TypedValidator
    { validator, validatorHash: vh, forwardingMPS, forwardingMPSHash }

-- | The hash of the validator.
typedValidatorHash :: forall (a :: Type). TypedValidator a -> ValidatorHash
typedValidatorHash = _.validatorHash <<< unwrap

-- | The validator script itself.
typedValidatorScript :: forall (a :: Type). TypedValidator a -> Validator
typedValidatorScript = _.validator <<< unwrap

-- | The minting policy that forwards all checks to the instance's validator.
forwardingMintingPolicy
  :: forall (a :: Type). TypedValidator a -> MintingPolicy
forwardingMintingPolicy = _.forwardingMPS <<< unwrap

-- We have a few functions, I'm not sure if we even need these for off chain
-- code:
-- -- Broken, see below (we need some notion of `applyCode`) https://github.com/Plutonomicon/cardano-transaction-lib/issues/24
-- -- | Make a `TypedValidator` (with no type constraints) from an untyped
-- -- |`Validator` script.
-- unsafeMkTypedValidator :: Scripts.Validator -> Maybe (TypedValidator Any)
-- unsafeMkTypedValidator validator = do
--   validatorHash <- validatorHash validator
--   let forwardingMPS = mkForwardingMintingPolicy validatorHash
--   forwardingMPSHash <- mintingPolicyHash fowardingMPS
--   TypedValidator
--       { validator
--       , validatorHash
--       , forwardingMPS
--       , forwardingMPSHash
--       }

-- -- mkTypedValidator, mkForwardingMintingPolicy and
-- --  mkTypedValidatorParam requires some notion
-- -- of compiled code and `applyCode` also
-- mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
-- mkForwardingMintingPolicy = undefined
