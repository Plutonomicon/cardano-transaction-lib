module Ctl.Internal.Types.ValidatorHash
  ( ValidatorHash(ValidatorHash)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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
