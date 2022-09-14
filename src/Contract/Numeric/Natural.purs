-- | Arbitrary precision natural numbers (backed by `BigInt`).
module CTL.Contract.Numeric.Natural (module Natural) where

import CTL.Internal.Types.Natural
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
