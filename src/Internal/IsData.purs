module CTL.Internal.IsData (class IsData) where

import CTL.Internal.FromData (class FromData)
import CTL.Internal.ToData (class ToData)

class IsData :: Type -> Constraint
class (FromData a, ToData a) <= IsData a

instance (FromData a, ToData a) => IsData a
