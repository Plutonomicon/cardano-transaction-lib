module Types.ScriptHash
  ( MintingPolicyHash(..)
  , ValidatorHash(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Serialization.Hash (ScriptHash)

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
