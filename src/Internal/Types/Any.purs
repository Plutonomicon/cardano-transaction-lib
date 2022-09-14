module CTL.Internal.Types.Any
  ( Any
  ) where

import Prelude (class Show)

-- Not sure if this is necessary but keep for now.
-- | For converting a subset of polymorphic types to that which will work with
-- | `PlutusData`.
foreign import data Any :: Type

instance Show Any where
  show _ = "(Any)"