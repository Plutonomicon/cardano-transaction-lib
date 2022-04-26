module TypeLevel.Nat where

import Type.Proxy
import Prelude

-- | Type level natural numbers. We need these for a variety of inductive "operations" (i.e. type classes w/ fundeps)
data Z

data S n

class KnownNat n where
  natVal :: Proxy n -> Int

instance KnownNat Z where
  natVal _ = 0

instance KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

-- | Less than or equal to.
class LTE :: Type -> Type -> Constraint
class (KnownNat n1, KnownNat n2) <= LTE n1 n2

instance KnownNat x => LTE Z x
else instance LTE Z (S Z)
else instance (KnownNat nl, KnownNat nr, LTE nl nr) => LTE (S nl) (S nr)
