-- | Arbitrary precision rational numbers (backed by `BigInt`).
module Contract.Numeric.Rational
  ( module Rational
  , module Ratio
  ) where

import Data.Ratio ((%), denominator, numerator, reduce) as Ratio
import Types.Rational (Rational) as Rational
