-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Contract.Numeric.Natural (module Natural) where

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
  ) as Natural
