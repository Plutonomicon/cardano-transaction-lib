module Equipartition
  ( class Equipartition
  , equipartition
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( fromArray
  , replicate
  , singleton
  , snoc
  , unsnoc
  ) as NEArray
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Maybe (Maybe(Just, Nothing))

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
        mapLastN (bi `mod` BigInt.fromInt numParts) (add one) $
          NEArray.replicate numParts (bi / BigInt.fromInt numParts)
        where
        -- | Applies the passed function to the last `n` elements of the 
        -- | `NonEmptyArray`.
        mapLastN
          :: forall (a :: Type) (n :: Type)
           . Ring n
          => Eq n
          => n
          -> (a -> a)
          -> NonEmptyArray a
          -> NonEmptyArray a
        mapLastN n f arr
          | n == zero = arr
          | otherwise =
              NEArray.unsnoc arr # \{ init, last } ->
                case NEArray.fromArray init of
                  Nothing ->
                    NEArray.singleton (f last)
                  Just init' ->
                    NEArray.snoc (mapLastN (n - one) f init') (f last)
