module TypeLevel.IndexedRecField (class IndexedRecField) where

import TypeLevel.Nat
import Data.Symbol

class (IsSymbol s, KnownNat n) <= IndexedRecField (t :: Type) s n | t s -> n
