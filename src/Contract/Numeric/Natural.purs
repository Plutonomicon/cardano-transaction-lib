-- | Arbitrary precision natural number (backed by BigInt).
module Contract.Numeric.Natural (module Natural) where

import Types.Natural
  ( Natural
  , fromBigInt
  , fromBigInt'
  , toBigInt
  ) as Natural