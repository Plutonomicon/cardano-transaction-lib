module Ctl.Internal.Types.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScript(PlutusScript)
  , PlutusScriptStakeValidator(PlutusScriptStakeValidator)
  , NativeScriptStakeValidator(NativeScriptStakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  , Language(PlutusV1, PlutusV2, PlutusV3)
  , plutusV1Script
  , plutusV2Script
  , plutusV3Script
  , stakeValidatorHashToBech32
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
  , encodeAeson
  , fromString
  , getField
  , toStringifiedNumbersJson
  )
import Control.Alt ((<|>))
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashToBech32Unsafe)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

data Language
  = PlutusV1
  | PlutusV2
  | PlutusV3

derive instance Eq Language
derive instance Ord Language
derive instance Generic Language _

instance DecodeAeson Language where
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected string")
    case _ of
      "PlutusV1" -> pure PlutusV1
      "PlutusV2" -> pure PlutusV2
      "PlutusV3" -> pure PlutusV3
      other -> Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
        other

instance EncodeAeson Language where
  encodeAeson = encodeAeson <<< case _ of
    PlutusV1 -> "PlutusV1"
    PlutusV2 -> "PlutusV2"
    PlutusV3 -> "PlutusV3"

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

plutusV3Script :: ByteArray -> PlutusScript
plutusV3Script ba = PlutusScript (ba /\ PlutusV3)

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

-- | `MintingPolicy` is a sum type of `PlutusScript` and `NativeScript` which are used as
-- | validators for minting constraints.
data MintingPolicy
  = PlutusMintingPolicy PlutusScript
  | NativeMintingPolicy NativeScript

derive instance Generic MintingPolicy _
derive instance Eq MintingPolicy

instance DecodeAeson MintingPolicy where
  decodeAeson aes =
    decodeAesonHelper "getPlutusMintingPolicy" PlutusMintingPolicy aes <|>
      decodeAesonHelper "getNativeMintingPolicy" NativeMintingPolicy aes

instance EncodeAeson MintingPolicy where
  encodeAeson (NativeMintingPolicy nscript) =
    encodeAeson { "getNativeMintingPolicy": nscript }
  encodeAeson (PlutusMintingPolicy script) =
    encodeAeson { "getPlutusMintingPolicy": script }

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
  encodeAeson (Validator script) =
    encodeAeson { "getValidator": script }

instance Show Validator where
  show = genericShow

-- | `NativeScriptStakeValidator`s are used as validators for withdrawals and
-- | stake address certificates.
newtype NativeScriptStakeValidator = NativeScriptStakeValidator NativeScript

derive instance Newtype NativeScriptStakeValidator _
derive instance Generic NativeScriptStakeValidator _
derive instance Eq NativeScriptStakeValidator

instance Show NativeScriptStakeValidator where
  show = genericShow

-- | `PlutusScriptStakeValidator`s are used as validators for withdrawals and
-- | stake address certificates.
newtype PlutusScriptStakeValidator = PlutusScriptStakeValidator PlutusScript

derive instance Newtype PlutusScriptStakeValidator _
derive instance Generic PlutusScriptStakeValidator _
derive instance Eq PlutusScriptStakeValidator

instance DecodeAeson PlutusScriptStakeValidator where
  decodeAeson = decodeAesonHelper "getStakeValidator" PlutusScriptStakeValidator

instance EncodeAeson PlutusScriptStakeValidator where
  encodeAeson (PlutusScriptStakeValidator script) =
    encodeAeson { "getStakeValidator": script }

instance Show PlutusScriptStakeValidator where
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
  encodeAeson (MintingPolicyHash hash) =
    encodeAeson { "getMintingPolicyHash": hash }

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
derive newtype instance ToData StakeValidatorHash
derive newtype instance FromData StakeValidatorHash

instance DecodeAeson StakeValidatorHash where
  decodeAeson = decodeAesonHelper "getStakeValidatorHash" StakeValidatorHash

instance EncodeAeson StakeValidatorHash where
  encodeAeson (StakeValidatorHash hash) =
    encodeAeson { "getStakeValidatorHash": hash }

instance Show StakeValidatorHash where
  show = genericShow

stakeValidatorHashToBech32 :: StakeValidatorHash -> Bech32String
stakeValidatorHashToBech32 = unwrap >>> scriptHashToBech32Unsafe "script"
