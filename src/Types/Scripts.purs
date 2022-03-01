module Types.Scripts
  ( MintingPolicy(..)
  , MintingPolicyHash(..)
  , PlutusScript(..)
  , StakeValidator(..)
  , StakeValidatorHash(..)
  , TypedValidator(..)
  , Validator(..)
  , ValidatorHash(..)
  , validatorAddress
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Serialization.Address
  ( Address
  , NetworkId
  , baseAddressToAddress
  , scriptAddress
  )
import Serialization.Hash (ScriptHash)
import Types.ByteArray (ByteArray)

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript ByteArray

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance Eq PlutusScript
derive newtype instance Ord PlutusScript

instance Show PlutusScript where
  show = genericShow

newtype MintingPolicyHash = MintingPolicyHash ScriptHash

derive instance Generic MintingPolicyHash _
derive instance Newtype MintingPolicyHash _
derive newtype instance Eq MintingPolicyHash
derive newtype instance Ord MintingPolicyHash

instance Show MintingPolicyHash where
  show = genericShow

newtype ValidatorHash = ValidatorHash ScriptHash

derive instance Generic ValidatorHash _
derive instance Newtype ValidatorHash _
derive newtype instance Eq ValidatorHash
derive newtype instance Ord ValidatorHash

instance Show ValidatorHash where
  show = genericShow

newtype StakeValidatorHash = StakeValidatorHash ScriptHash

derive instance Generic StakeValidatorHash _
derive instance Newtype StakeValidatorHash _
derive newtype instance Eq StakeValidatorHash
derive newtype instance Ord StakeValidatorHash

instance Show StakeValidatorHash where
  show = genericShow

-- | `MintingPolicy` is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy PlutusScript

derive instance Generic MintingPolicy _
derive instance Newtype MintingPolicy _
derive newtype instance Eq MintingPolicy
derive newtype instance Ord MintingPolicy

instance Show MintingPolicy where
  show = genericShow

newtype Validator = Validator PlutusScript

derive instance Generic Validator _
derive instance Newtype Validator _
derive newtype instance Eq Validator
derive newtype instance Ord Validator

instance Show Validator where
  show = genericShow

-- | `StakeValidator` is a wrapper around 'Script's which are used as validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator PlutusScript

derive instance Generic StakeValidator _
derive instance Newtype StakeValidator _
derive newtype instance Eq StakeValidator
derive newtype instance Ord StakeValidator

instance Show StakeValidator where
  show = genericShow

-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- | A typed validator script with its `ValidatorScript` and `Address`.
newtype TypedValidator (a :: Type) = TypedValidator
  { validator :: Validator
  , validatorHash :: ValidatorHash
  , forwardingMPS :: MintingPolicy
  , forwardingMPSHash :: MintingPolicyHash
  -- The hash of the minting policy that checks whether the validator
  -- is run in this transaction
  }

derive instance Generic (TypedValidator a) _
derive newtype instance Eq (TypedValidator a)

instance Show (TypedValidator a) where
  show = genericShow

-- | The address of the typed validator.
validatorAddress :: forall (a :: Type). NetworkId -> TypedValidator a -> Address
validatorAddress networkId (TypedValidator typedVal) =
  baseAddressToAddress $ scriptAddress networkId $ unwrap typedVal.validatorHash