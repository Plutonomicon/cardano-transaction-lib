-- | Arbitrary precision rational numbers (backed by `BigInt`).
module CTL.Contract.Numeric.Rational
  ( module Rational
  ) where

import CTL.Internal.Types.Rational
  ( class RationalComponent
  , Rational
  , denominator
  , denominatorAsNat
  , numerator
  , recip
  , reduce
  , (%)
  ) as Rational
