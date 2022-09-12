module IsData (class IsData) where

import FromData (class FromData)
import ToData (class ToData)

class IsData :: Type -> Constraint
class (FromData a, ToData a) <= IsData a

instance (FromData a, ToData a) => IsData a
