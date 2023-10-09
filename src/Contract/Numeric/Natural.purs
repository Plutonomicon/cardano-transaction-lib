-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Contract.Numeric.Natural (module X) where

import Ctl.Internal.Types.Natural
  ( Natural
  , binaryOnBigInt
  , fromBigInt
  , fromBigInt'
  , fromInt
  , fromInt'
  , fromString
  , minus
  , toBigInt
  , (^-)
  ) as X
