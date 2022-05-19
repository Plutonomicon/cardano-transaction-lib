module Types.Scripts
  ( MintingPolicy(..)
  , MintingPolicyHash(..)
  , PlutusScript(..)
  , StakeValidator(..)
  , StakeValidatorHash(..)
  , Validator(..)
  , ValidatorHash(..)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAesonViaJson
  , encodeAesonViaJson
  )
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , decodeJson
  , encodeJson
  , getField
  , Json
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Hash (ScriptHash)
import ToData (class ToData)
import Types.ByteArray (ByteArray)

--------------------------------------------------------------------------------
-- `PlutusScript` newtypes and `TypedValidator`
--------------------------------------------------------------------------------
-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript ByteArray

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance Eq PlutusScript
derive newtype instance Ord PlutusScript
derive newtype instance DecodeJson PlutusScript
derive newtype instance EncodeJson PlutusScript
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Show PlutusScript where
  show = genericShow

decodeJsonHelper
  ∷ ∀ (a ∷ Type) (b :: Type)
   . DecodeJson a
  => String
  → (a -> b)
  → Json
  → Either JsonDecodeError b
decodeJsonHelper constrName constr = caseJsonObject
  (Left $ TypeMismatch "Expected object")
  (flip getField constrName >=> decodeJson >>> map constr)

-- | `MintingPolicy` is a wrapper around `PlutusScript`s which are used as
-- | validators for minting constraints.
newtype MintingPolicy = MintingPolicy PlutusScript

derive instance Generic MintingPolicy _
derive instance Newtype MintingPolicy _
derive newtype instance Eq MintingPolicy
derive newtype instance Ord MintingPolicy

instance DecodeJson MintingPolicy where
  decodeJson = decodeJsonHelper "getMintingPolicy" MintingPolicy

instance EncodeJson MintingPolicy where
  encodeJson (MintingPolicy script) = encodeJson
    { "getMintingPolicy": encodeJson script }

instance DecodeAeson MintingPolicy where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson MintingPolicy where
  encodeAeson' = encodeAesonViaJson

instance Show MintingPolicy where
  show = genericShow

newtype Validator = Validator PlutusScript

derive instance Generic Validator _
derive instance Newtype Validator _
derive newtype instance Eq Validator
derive newtype instance Ord Validator

instance DecodeJson Validator where
  decodeJson = decodeJsonHelper "getValidator" Validator

instance EncodeJson Validator where
  encodeJson (Validator script) = encodeJson
    { "getValidator": encodeJson script }

instance DecodeAeson Validator where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson Validator where
  encodeAeson' = encodeAesonViaJson

instance Show Validator where
  show = genericShow

-- | `StakeValidator` is a wrapper around `PlutusScript`s which are used as
-- | validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator PlutusScript

derive instance Generic StakeValidator _
derive instance Newtype StakeValidator _
derive newtype instance Eq StakeValidator
derive newtype instance Ord StakeValidator

instance DecodeJson StakeValidator where
  decodeJson = decodeJsonHelper "getStakeValidator" StakeValidator

instance EncodeJson StakeValidator where
  encodeJson (StakeValidator script) = encodeJson
    { "getStakeValidator": encodeJson script }

instance DecodeAeson StakeValidator where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson StakeValidator where
  encodeAeson' = encodeAesonViaJson

instance Show StakeValidator where
  show = genericShow

--------------------------------------------------------------------------------
-- `ScriptHash` newtypes
--------------------------------------------------------------------------------
newtype MintingPolicyHash = MintingPolicyHash ScriptHash

derive instance Generic MintingPolicyHash _
derive instance Newtype MintingPolicyHash _
derive newtype instance Eq MintingPolicyHash
derive newtype instance Ord MintingPolicyHash
derive newtype instance FromData MintingPolicyHash
derive newtype instance ToData MintingPolicyHash
derive newtype instance FromMetadata MintingPolicyHash
derive newtype instance ToMetadata MintingPolicyHash

instance DecodeJson MintingPolicyHash where
  decodeJson = decodeJsonHelper "getMintingPolicyHash" MintingPolicyHash

instance EncodeJson MintingPolicyHash where
  encodeJson (MintingPolicyHash hash) = encodeJson
    { "getMintingPolicyHash": encodeJson hash }

instance DecodeAeson MintingPolicyHash where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson MintingPolicyHash where
  encodeAeson' = encodeAesonViaJson

instance Show MintingPolicyHash where
  show = genericShow

newtype ValidatorHash = ValidatorHash ScriptHash

derive instance Generic ValidatorHash _
derive instance Newtype ValidatorHash _
derive newtype instance Eq ValidatorHash
derive newtype instance Ord ValidatorHash
derive newtype instance FromData ValidatorHash
derive newtype instance ToData ValidatorHash
derive newtype instance FromMetadata ValidatorHash
derive newtype instance ToMetadata ValidatorHash

instance DecodeJson ValidatorHash where
  decodeJson = decodeJsonHelper "getValidatorHash" ValidatorHash

instance EncodeJson ValidatorHash where
  encodeJson (ValidatorHash hash) = encodeJson
    { "getValidatorHash": encodeJson hash }

instance DecodeAeson ValidatorHash where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson ValidatorHash where
  encodeAeson' = encodeAesonViaJson

instance Show ValidatorHash where
  show = genericShow

newtype StakeValidatorHash = StakeValidatorHash ScriptHash

derive instance Generic StakeValidatorHash _
derive instance Newtype StakeValidatorHash _
derive newtype instance Eq StakeValidatorHash
derive newtype instance Ord StakeValidatorHash

instance DecodeJson StakeValidatorHash where
  decodeJson = decodeJsonHelper "getStakeValidatorHash" StakeValidatorHash

instance EncodeJson StakeValidatorHash where
  encodeJson (StakeValidatorHash hash) = encodeJson
    { "getStakeValidatorHash": encodeJson hash }

instance DecodeAeson StakeValidatorHash where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson StakeValidatorHash where
  encodeAeson' = encodeAesonViaJson

instance Show StakeValidatorHash where
  show = genericShow
