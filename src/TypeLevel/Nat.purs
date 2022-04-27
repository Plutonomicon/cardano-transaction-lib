module TypeLevel.Nat
  ( Z
  , S
  , class KnownNat
  , natVal
  ) where

import Type.Proxy
import Prelude ((+))

-- | Type level natural numbers. We need these for a variety of inductive "operations" (i.e. type classes w/ fundeps)
data Z

data S n

-- | PureScript version of the Haskell class defined in GHC.TypeLits. This is the Nat version of IsSymbol, more or less.
class KnownNat n where
  natVal :: Proxy n -> Int

instance KnownNat Z where
  natVal _ = 0

instance KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)
