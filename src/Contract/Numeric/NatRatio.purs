-- | A module for constructing well-defined ratios of natural numbers (with
-- | non-zero denominator), potentially via cancellation of negative sign.
module CTL.Contract.Numeric.NatRatio (module NatRatio) where

import CTL.Internal.Types.NatRatio
  ( NatRatio
  , denominator
  , denominatorAsNat
  , fromBigInts
  , fromNaturals
  , fromRational
  , numerator
  , numeratorAsNat
  , toRational
  ) as NatRatio