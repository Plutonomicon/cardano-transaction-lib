-- | Generic interactions with cardano-serialization-lib
module Serialization.Csl where

import Control.Category ((>>>))
import Unsafe.Coerce (unsafeCoerce)

foreign import data CslType :: Type

class ToCsl t frgn | t -> frgn where
  toCslRep :: t -> frgn

class FromCsl t frgn | frgn -> t where
  fromCslRep :: frgn -> t

toCslType :: forall t f. ToCsl t f => t -> CslType
toCslType = toCslRep >>> unsafeCoerce
