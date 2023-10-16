module Ctl.Internal.Partition
  ( class Equipartition
  , class Partition
  , equipartition
  , partition
  ) where

import Prelude

import Data.Array (replicate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( appendArray
  , fromArray
  , range
  , replicate
  , singleton
  , sortBy
  , zip
  , zipWith
  ) as NEArray
import Data.Foldable (any, length, sum)
import Data.Function (on)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ordering (invert) as Ordering
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toInt) as BigInt
import Partial.Unsafe (unsafePartial)

class Partition (a :: Type) where
  partition :: a -> NonEmptyArray a -> Maybe (NonEmptyArray a)

-- | Partitions a `BigInt` into a number of parts, where the size of each part
-- | is proportional to the size of its corresponding element in the given
-- | list of weights, and the number of parts is equal to the number of weights.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/14e0f1c2a457f85b8ea470661e7bec5e6bcf93e0/lib/numeric/src/Cardano/Numeric/Util.hs#L175
instance Partition BigInt where
  partition target weights
    | any (_ < zero) weights = Nothing
    | sum weights == zero = Nothing
    | otherwise = Just portionsRounded
        where
        portionsRounded :: NonEmptyArray BigInt
        portionsRounded
          -- 1. Start with the list of unrounded portions:
          = portionsUnrounded
          # map QuotRem
          -- 2. Attach an index to each portion, so that we can remember the
          -- original order:
          # NEArray.zip (NEArray.range 1 $ length portionsUnrounded)
          -- 3. Sort the portions in descending order of their remainders, and
          -- then sort each subsequence with equal remainders into descending
          -- order of their integral parts:
          --
          -- NOTE: We sort unrounded portions by comparing their remainders
          -- and not their fractional parts, as implemented in cardano-wallet.
          -- This serves the same purpose, namely to distribute the `shortfall`
          -- fairly between the portions, rounding *up* those portions that
          -- have a larger remainder (i.e. larger fractional part).
          # NEArray.sortBy ((\x -> Ordering.invert <<< compare x) `on` snd)
          -- 4. Apply pre-computed roundings to each portion:
          # round
          -- 5. Restore the original order:
          # NEArray.sortBy (comparing fst)
          -- 6. Strip away the indices:
          # map snd

        round
          :: NonEmptyArray (Int /\ QuotRem BigInt)
          -> NonEmptyArray (Int /\ BigInt)
        round portions =
          NEArray.zipWith (+)
            (map (fst <<< unwrap) <$> portions)
            ( fromArrayUnsafe $
                replicate shortfall one
                  <> replicate (length portions - shortfall) zero
            )

        shortfall :: Int
        shortfall = toIntUnsafe $ target - sum (map fst portionsUnrounded)

        portionsUnrounded :: NonEmptyArray (BigInt /\ BigInt)
        portionsUnrounded = weights <#> \w -> (target * w) `quotRem` sumWeights

        sumWeights :: BigInt
        sumWeights = sum weights

-- | Represents types whose values can be equally divided into several parts.
class Equipartition (a :: Type) where
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
          quot /\ rem = toIntUnsafe <$> (bi `quotRem` BigInt.fromInt numParts)
        in
          NEArray.replicate (numParts - rem) quot
            `NEArray.appendArray` replicate rem (quot + one)

toIntUnsafe :: BigInt -> Int
toIntUnsafe = unsafePartial fromJust <<< BigInt.toInt

fromArrayUnsafe :: forall (a :: Type). Array a -> NonEmptyArray a
fromArrayUnsafe = unsafePartial fromJust <<< NEArray.fromArray

quotRem :: forall (a :: Type). EuclideanRing a => a -> a -> (a /\ a)
quotRem a b = (a `div` b) /\ (a `mod` b)

newtype QuotRem (a :: Type) = QuotRem (a /\ a)

derive instance Newtype (QuotRem a) _
derive newtype instance Eq a => Eq (QuotRem a)

instance Ord a => Ord (QuotRem a) where
  compare (QuotRem (quot0 /\ rem0)) (QuotRem (quot1 /\ rem1)) =
    case compare rem0 rem1 of
      EQ -> compare quot0 quot1
      ordering -> ordering
