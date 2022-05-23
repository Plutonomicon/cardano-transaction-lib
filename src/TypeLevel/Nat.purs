module TypeLevel.Nat
  ( Z
  , S
  , Nat
  , class KnownNat
  , natVal
  ) where

import Type.Proxy (Proxy(Proxy))
import Prelude ((+))

-- | The kind of type level natural number. This is not strictly needed, but improves the
-- | legibility of type errors and facilitates more comprehensible type signatures
data Nat :: Type
data Nat

-- | Type level natural numbers. We need these for a variety of inductive "operations" (i.e. type classes w/ fundeps)
foreign import data Z :: Nat

foreign import data S :: Nat -> Nat

-- | PureScript version of the Haskell class defined in GHC.TypeLits. This is the Nat version of IsSymbol, more or less.
class KnownNat :: Nat -> Constraint
class KnownNat n where
  natVal :: Proxy n -> Int

instance KnownNat Z where
  natVal _ = 0

instance KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)
