-- | Arbitrary precision rational numbers (backed by `BigInt`).
module Contract.Numeric.Rational
  ( module Rational
  ) where

import Types.Rational
  ( Rational
  , class RationalComponent
  , reduce
  , (%)
  , recip
  , numerator
  , denominator
  , denominatorAsNat
  ) as Rational
