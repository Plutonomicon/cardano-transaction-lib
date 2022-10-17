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
  , range
  , replicate
  , singleton
  , sortBy
  , zip
  , zipWith
  ) as NEArray
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Foldable (any, length, sum)
import Data.Function (on)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ordering (invert) as Ordering
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

class Partition (a :: Type) where
  partition :: a -> NonEmptyArray a -> Maybe (NonEmptyArray a)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/14e0f1c2a457f85b8ea470661e7bec5e6bcf93e0/lib/numeric/src/Cardano/Numeric/Util.hs#L175
instance Partition BigInt where
  partition target weights
    | any (\w -> w < zero) weights = Nothing
    | sum weights == zero = Nothing
    | otherwise = Just portionsRounded
        where
        portionsRounded :: NonEmptyArray BigInt
        portionsRounded = portionsUnrounded
          # map QuotRem
          # NEArray.zip (NEArray.range 1 $ length portionsUnrounded)
          # NEArray.sortBy ((\x -> Ordering.invert <<< compare x) `on` snd)
          # round
          # NEArray.sortBy (comparing fst)
          # map snd

        round
          :: NonEmptyArray (Int /\ QuotRem BigInt)
          -> NonEmptyArray (Int /\ BigInt)
        round portions =
          NEArray.zipWith (+)
            (map (fst <<< unwrap) <$> portions)
            ( NEArray.replicate shortfall one
                <> NEArray.replicate (length portions - shortfall) zero
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
