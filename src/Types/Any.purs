module Types.Any
  ( Any(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- Not sure if this is necessary but keep for now.
-- | For converting a subset of polymorphic types to that which will work with
-- | `PlutusData`.
data Any

derive instance Generic Any _
derive instance Eq Any

instance Show Any where
  show = genericShow