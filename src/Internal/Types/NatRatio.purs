-- | A module for constructing well-defined ratios of natural numbers (with
-- | non-zero denominator), potentially via cancellation of negative sign.
module Ctl.Internal.Types.NatRatio
  ( NatRatio
  , denominator
  , fromBigInts
  , fromRational
  , numerator
  , toRational
  ) where

import Prelude

import Ctl.Internal.Types.Rational (Rational)
import Ctl.Internal.Types.Rational
  ( denominator
  , numerator
  , (%)
  ) as Rational
import Data.Maybe (Maybe(Nothing))
import JS.BigInt (BigInt)

-- | `NatRatio` is a newtype over `Rational` with smart constructors to create a
-- | `NatRatio` safely. Therefore, the data constructor is not exported.
newtype NatRatio = NatRatio Rational

derive newtype instance Eq NatRatio
derive newtype instance Ord NatRatio
-- If `NatRatio`s are safely constructed, then the underlying `Rational`s
-- should obey Semiring laws.
derive newtype instance Semiring NatRatio

instance Show NatRatio where
  show (NatRatio r) = "(NatRatio " <> show r <> ")"

-- | Fails with `Nothing` if the denominator is zero or the overall sign of
-- | the rational numbers is negative.
fromRational :: Rational -> Maybe NatRatio
fromRational r = fromBigInts (Rational.numerator r) (Rational.denominator r)

-- | Converts a `NatRatio` to the underlying `Rational`.
toRational :: NatRatio -> Rational
toRational (NatRatio r) = r

-- | Given two `BigInt`s, attempts to convert to a `NatRatio`. Fails if the
-- | denominator is `zero` or the overall sign is negative.
fromBigInts :: BigInt -> BigInt -> Maybe NatRatio
fromBigInts n d
  | (n < zero) == (d < zero) = NatRatio <$> n Rational.% d
  | otherwise = Nothing

-- | Get the numerator of a `NatRatio` as `BigInt`.
numerator :: NatRatio -> BigInt
numerator (NatRatio r) = Rational.numerator r

-- | Get the denominator of a `NatRatio` as `BigInt`.
denominator :: NatRatio -> BigInt
denominator (NatRatio r) = Rational.denominator r
