module Ctl.Internal.Types.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , PlutusScriptStakeValidator(PlutusScriptStakeValidator)
  , NativeScriptStakeValidator(NativeScriptStakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  , stakeValidatorHashToBech32
  , module X
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.Types.Language (Language(..), fromCsl, toCsl) as X
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.PlutusScript (PlutusScript(..))
import Control.Alt ((<|>))
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashToBech32Unsafe)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- `PlutusScript` newtypes and `TypedValidator`
--------------------------------------------------------------------------------

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
stakeValidatorHashToBech32 = unsafePartial $ unwrap >>> scriptHashToBech32Unsafe
  "script"
