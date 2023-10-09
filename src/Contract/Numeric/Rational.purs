-- | Arbitrary precision rational numbers (backed by `BigInt`).
module Contract.Numeric.Rational
  ( module X
  ) where

import Ctl.Internal.Types.Rational
  ( class RationalComponent
  , Rational
  , denominator
  , denominatorAsNat
  , numerator
  , recip
  , reduce
  , (%)
  ) as X
