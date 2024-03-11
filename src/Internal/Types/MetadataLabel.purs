module Ctl.Internal.Types.MetadataLabel where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Types (BigNum)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype MetadataLabel = MetadataLabel BigNum

derive instance Newtype MetadataLabel _
derive newtype instance AsCbor MetadataLabel
derive instance Eq MetadataLabel
derive instance Generic MetadataLabel _
derive instance Ord MetadataLabel

instance Show MetadataLabel where
  show = genericShow
