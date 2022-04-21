-- | A module for constructing well-defined ratios of natural numbers (with
-- | non-zero denominator), potentially via cancellation of negative sign.
module Types.NatRatio
  ( NatRatio
  , denominator
  , denominatorAsNat
  , fromBigInts
  , fromNaturals
  , fromRational
  , numerator
  , numeratorAsNat
  , toRational
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(Just, Nothing))
import Types.Natural (Natural, fromBigInt', toBigInt)
import Types.Rational
  ( Rational
  , (%)
  , numerator
  , denominator
  , denominatorAsNat
  ) as R

-- | `NatRatio` is a newtype over `Rational` with smart constructors to create a
-- | `NatRatio` safely. Therefore, the data constructor is not exported.
newtype NatRatio = NatRatio R.Rational

derive newtype instance Eq NatRatio
derive newtype instance Ord NatRatio
-- If `NatRatio`s are safely constructed, then the underlying `Rational`s
-- should obey Semiring laws.
derive newtype instance Semiring NatRatio

instance Show NatRatio where
  show (NatRatio r) = "(NatRatio " <> show r <> ")"

-- | Fails with `Nothing` if the denominator is zero or the overall sign of
-- | the rational numbers is negative.
fromRational :: R.Rational -> Maybe NatRatio
fromRational r = fromBigInts (R.numerator r) (R.denominator r)

-- | Converts a `NatRatio` to the underlying `Rational`.
toRational :: NatRatio -> R.Rational
toRational (NatRatio r) = r

-- | Given two `Natural`s, attempts to convert to a `NatRatio`. Fails if the
-- | denominator is `zero`.
fromNaturals :: Natural -> Natural -> Maybe NatRatio
fromNaturals n d = NatRatio <$> toBigInt n R.% toBigInt d

-- | Given two `BigInt`s, attempts to convert to a `NatRatio`. Fails if the
-- | denominator is `zero` or the overall sign is negative.
fromBigInts :: BigInt -> BigInt -> Maybe NatRatio
fromBigInts n d
  | (n < zero) == (d < zero) = NatRatio <$> n R.% d
  | otherwise = Nothing

-- | Get the numerator of a `NatRatio` as `BigInt`.
numerator :: NatRatio -> BigInt
numerator (NatRatio r) = R.numerator r

-- This is safe because the numerator is guaranteed to be non-negative.
-- | Get the numerator of a `NatRatio` as `Natural`.
numeratorAsNat :: NatRatio -> Natural
numeratorAsNat (NatRatio r) = fromBigInt' $ R.numerator r

-- | Get the denominator of a `NatRatio` as `BigInt`.
denominator :: NatRatio -> BigInt
denominator (NatRatio r) = R.denominator r

-- This is safe because the denominator is guaranteed to be positive.
-- | Get the denominator of a `NatRatio` as `Natural`.
denominatorAsNat :: NatRatio -> Natural
denominatorAsNat (NatRatio r) = R.denominatorAsNat r
