-- | Arbitrary precision rational numbers (backed by `BigInt`).
module CTL.Contract.Numeric.Rational
  ( module Rational
  ) where

import CTL.Internal.Types.Rational
  ( Rational
  , class RationalComponent
  , reduce
  , (%)
  , recip
  , numerator
  , denominator
  , denominatorAsNat
  ) as Rational
