module Types.Scripts
  ( MintingPolicy(MintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScript(PlutusScript)
  , StakeValidator(StakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  , Language(PlutusV1, PlutusV2)
  , plutusV1Script
  , plutusV2Script
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson'
  , getField
  , toStringifiedNumbersJson
  , fromString
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Hash (ScriptHash)
import ToData (class ToData)
import Types.ByteArray (ByteArray)

data Language
  = PlutusV1
  | PlutusV2

derive instance Eq Language
derive instance Ord Language
derive instance Generic Language _

instance DecodeAeson Language where
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected string")
    case _ of
      "PlutusV1" -> pure PlutusV1
      "PlutusV2" -> pure PlutusV2
      other -> Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
        other

instance EncodeAeson Language where
  encodeAeson' = pure <<< fromString <<< case _ of
    PlutusV1 -> "PlutusV1"
    PlutusV2 -> "PlutusV2"

instance Show Language where
  show = genericShow

--------------------------------------------------------------------------------
-- `PlutusScript` newtypes and `TypedValidator`
--------------------------------------------------------------------------------
-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript (ByteArray /\ Language)

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance Eq PlutusScript
derive newtype instance Ord PlutusScript
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Show PlutusScript where
  show = genericShow

plutusV1Script :: ByteArray -> PlutusScript
plutusV1Script ba = PlutusScript (ba /\ PlutusV1)

plutusV2Script :: ByteArray -> PlutusScript
plutusV2Script ba = PlutusScript (ba /\ PlutusV2)

decodeAesonHelper
  :: ∀ (a :: Type) (b :: Type)
   . DecodeAeson a
  => String
  -> (a -> b)
  -> Aeson
  -> Either JsonDecodeError b
decodeAesonHelper constrName constr = caseAesonObject
  (Left $ TypeMismatch "Expected object")
  (flip getField constrName >=> decodeAeson >>> map constr)

-- | `MintingPolicy` is a wrapper around `PlutusScript`s which are used as
-- | validators for minting constraints.
newtype MintingPolicy = MintingPolicy PlutusScript

derive instance Generic MintingPolicy _
derive instance Newtype MintingPolicy _
derive newtype instance Eq MintingPolicy
derive newtype instance Ord MintingPolicy

instance DecodeAeson MintingPolicy where
  decodeAeson = decodeAesonHelper "getMintingPolicy" MintingPolicy

instance EncodeAeson MintingPolicy where
  encodeAeson' (MintingPolicy script) = do
    encodeAeson' { "getMintingPolicy": script }

instance Show MintingPolicy where
  show = genericShow

newtype Validator = Validator PlutusScript

derive instance Generic Validator _
derive instance Newtype Validator _
derive newtype instance Eq Validator
derive newtype instance Ord Validator

instance DecodeAeson Validator where
  decodeAeson = decodeAesonHelper "getValidator" Validator

instance EncodeAeson Validator where
  encodeAeson' (Validator script) =
    encodeAeson' { "getValidator": script }

instance Show Validator where
  show = genericShow

-- | `StakeValidator` is a wrapper around `PlutusScript`s which are used as
-- | validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator PlutusScript

derive instance Generic StakeValidator _
derive instance Newtype StakeValidator _
derive newtype instance Eq StakeValidator
derive newtype instance Ord StakeValidator

instance DecodeAeson StakeValidator where
  decodeAeson = decodeAesonHelper "getStakeValidator" StakeValidator

instance EncodeAeson StakeValidator where
  encodeAeson' (StakeValidator script) =
    encodeAeson' { "getStakeValidator": script }

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

instance DecodeAeson MintingPolicyHash where
  decodeAeson = decodeAesonHelper "getMintingPolicyHash" MintingPolicyHash

instance EncodeAeson MintingPolicyHash where
  encodeAeson' (MintingPolicyHash hash) =
    encodeAeson' { "getMintingPolicyHash": hash }

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
derive newtype instance EncodeAeson ValidatorHash
derive newtype instance DecodeAeson ValidatorHash

instance Show ValidatorHash where
  show = genericShow

newtype StakeValidatorHash = StakeValidatorHash ScriptHash

derive instance Generic StakeValidatorHash _
derive instance Newtype StakeValidatorHash _
derive newtype instance Eq StakeValidatorHash
derive newtype instance Ord StakeValidatorHash

instance DecodeAeson StakeValidatorHash where
  decodeAeson = decodeAesonHelper "getStakeValidatorHash" StakeValidatorHash

instance EncodeAeson StakeValidatorHash where
  encodeAeson' (StakeValidatorHash hash) =
    encodeAeson' { "getStakeValidatorHash": hash }

instance Show StakeValidatorHash where
  show = genericShow
