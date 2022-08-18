-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Contract.Numeric.Natural (module Natural) where

import Types.Natural
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
