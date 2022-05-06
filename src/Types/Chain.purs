module Types.Chain
  ( Tip(..)
  , BlockHeaderHash(..)
  ) where

import Prelude

import Serialization.Address (Slot)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Tip
  = TipAtGenesis
  | Tip Slot BlockHeaderHash

derive instance Generic Tip _
derive instance Eq Tip

instance Show Tip where
  show = genericShow

newtype BlockHeaderHash = BlockHeaderHash String

derive instance Generic BlockHeaderHash _
derive instance Newtype BlockHeaderHash _
derive newtype instance Eq BlockHeaderHash

instance Show BlockHeaderHash where
  show = genericShow
