module Ctl.Internal.IsData (class IsData) where

import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.ToData (class ToData)

class IsData :: Type -> Constraint
class (FromData a, ToData a) <= IsData a

instance (FromData a, ToData a) => IsData a
