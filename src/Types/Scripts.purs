module Types.Scripts
  ( MintingPolicy(..)
  , MintingPolicyHash(..)
  , PlutusScript(..)
  , Validator(..)
  , ValidatorHash(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Serialization.Hash (ScriptHash)
import Types.ByteArray (ByteArray)

newtype MintingPolicyHash = MintingPolicyHash ScriptHash

derive instance genericMintingPolicyHash :: Generic MintingPolicyHash _
derive instance newtypeMintingPolicyHash :: Newtype MintingPolicyHash _
derive newtype instance eqMintingPolicyHash :: Eq MintingPolicyHash

instance showMintingPolicyHash :: Show MintingPolicyHash where
  show = genericShow

newtype ValidatorHash = ValidatorHash ScriptHash

derive instance Generic ValidatorHash _
derive instance Newtype ValidatorHash _
derive newtype instance Eq ValidatorHash

instance Show ValidatorHash where
  show = genericShow

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript ByteArray

derive instance genericPlutusScript :: Generic PlutusScript _
derive instance newtypePlutusScript :: Newtype PlutusScript _
derive newtype instance eqPlutusScript :: Eq PlutusScript
derive newtype instance ordPlutusScript :: Ord PlutusScript

instance showPlutusScript :: Show PlutusScript where
  show = genericShow

newtype MintingPolicy = MintingPolicy PlutusScript

derive instance genericMintingPolicy :: Generic MintingPolicy _
derive instance newtypeMintingPolicy :: Newtype MintingPolicy _
derive newtype instance eqMintingPolicy :: Eq MintingPolicy
derive newtype instance ordMintingPolicy :: Ord MintingPolicy

instance showMintingPolicy :: Show MintingPolicy where
  show = genericShow

newtype Validator = Validator PlutusScript

derive instance genericValidator :: Generic Validator _
derive instance newtypeValidator :: Newtype Validator _
derive newtype instance eqValidator :: Eq Validator
derive newtype instance ordValidator :: Ord Validator

instance showValidator :: Show Validator where
  show = genericShow