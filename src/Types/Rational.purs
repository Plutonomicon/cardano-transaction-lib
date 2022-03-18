module Types.Rational
  ( Rational
  ) where

import Data.Ratio (Ratio)
import Data.BigInt (BigInt)

-- Note that the underlying `Data.Ratio` does not prevent a zero denominator.
-- We could keep this behaviour internally and provide safety with `ToData`
-- & `FromData` or rewrite this to provide extra safety.
type Rational = Ratio BigInt
