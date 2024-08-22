module Ctl.Internal.IsData (class IsData) where

import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)

class IsData :: Type -> Constraint
class (FromData a, ToData a) <= IsData a

instance (FromData a, ToData a) => IsData a
