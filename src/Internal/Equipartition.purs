module Ctl.Internal.Equipartition
  ( class Equipartition
  , equipartition
  ) where

import Prelude

import Data.Array (replicate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (appendArray, replicate, singleton) as NEArray
import Data.Maybe (fromJust)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toInt) as BigInt
import Partial.Unsafe (unsafePartial)

-- | Represents types whose values can be equally divided into several parts.
class Equipartition a where
  equipartition :: a -> Int -> NonEmptyArray a

-- | Computes the equipartition of a `BigInt` into `numParts` smaller `BigInt`s
-- | whose values differ by no more than 1. The resultant array is sorted in
-- | ascending order.
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/d4b30de073f2b5eddb25bf12c2453abb42e8b352/lib/numeric/src/Cardano/Numeric/Util.hs#L127
instance Equipartition BigInt where
  equipartition bi numParts
    | numParts <= one =
        NEArray.singleton bi
    | otherwise =
        let
          quot = bi / BigInt.fromInt numParts
          rem = toIntUnsafe (bi `mod` BigInt.fromInt numParts)
        in
          NEArray.replicate (numParts - rem) quot
            `NEArray.appendArray` replicate rem (quot + one)
        where
        toIntUnsafe :: BigInt -> Int
        toIntUnsafe = unsafePartial fromJust <<< BigInt.toInt
