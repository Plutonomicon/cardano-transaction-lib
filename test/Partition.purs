module Test.Ctl.Partition (suite) where

import Prelude

import Ctl.Internal.Partition
  ( class Equipartition
  , class Partition
  , equipartition
  , partition
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Array (elem) as Array
import Data.Array.NonEmpty (NonEmptyArray, (:))
import Data.Array.NonEmpty (length, singleton, sort, zip) as NEArray
import Data.Foldable (all, foldMap, sum)
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec.QuickCheck (quickCheck, quickCheck')

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "Partition" do
    group "partitionBigInt" do
      test "prop_partitionBigInt_pos_weights"
        (quickCheck prop_partitionBigInt_pos_weights)

      test "prop_partitionBigInt_sum_weights"
        (quickCheck prop_partitionBigInt_sum_weights)

      test "prop_partitionBigInt_length"
        (quickCheck prop_partitionBigInt_length)

      test "prop_partitionBigInt_sum"
        (quickCheck prop_partitionBigInt_sum)

      test "prop_partitionBigInt_fair"
        (quickCheck prop_partitionBigInt_fair)

    group "equipartitionBigInt" do
      test "prop_equipartitionBigInt_trivial"
        (quickCheck prop_equipartitionBigInt_trivial)

      test "prop_equipartitionBigInt_length"
        (quickCheck prop_equipartitionBigInt_length)

      test "prop_equipartitionBigInt_sum"
        (quickCheck' 10 prop_equipartitionBigInt_sum)

      test "prop_equipartitionBigInt_order"
        (quickCheck' 10 prop_equipartitionBigInt_order)

      test "prop_equipartitionBigInt_fair"
        (quickCheck' 10 prop_equipartitionBigInt_fair)

prop_partitionBigInt_pos_weights
  :: BigInt' -> BigIntNeg -> NonEmptyArray BigIntGeqOne -> Boolean
prop_partitionBigInt_pos_weights bi negWeight weights =
  isNothing $ partition bi (unwrap negWeight : map unwrap weights)

prop_partitionBigInt_sum_weights :: BigInt' -> NonEmptyArray BigInt' -> Boolean
prop_partitionBigInt_sum_weights bi =
  isNothing <<< partition bi <<< map (const (zero :: BigInt'))

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/cacdb55a5de6857ef666c4a6eac662de6952b5a6/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L142
prop_partitionBigInt_length :: BigInt' -> NonEmptyArray BigIntGeqOne -> Boolean
prop_partitionBigInt_length bi weights =
  map NEArray.length (partition bi $ map unwrap weights)
    == Just (NEArray.length weights)

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/cacdb55a5de6857ef666c4a6eac662de6952b5a6/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L151
prop_partitionBigInt_sum :: BigInt' -> NonEmptyArray BigIntGeqOne -> Boolean
prop_partitionBigInt_sum bi weights =
  map sum (partition bi $ map unwrap weights) == Just bi

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/cacdb55a5de6857ef666c4a6eac662de6952b5a6/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L162
prop_partitionBigInt_fair :: BigInt' -> NonEmptyArray BigIntGeqOne -> Boolean
prop_partitionBigInt_fair bi weights' =
  case partition bi weights of
    Nothing -> false
    Just portions ->
      all (\(x /\ a) -> x >= a) (NEArray.zip portions portionsLowerBounds)
        && all (\(x /\ b) -> x <= b) (NEArray.zip portions portionsUpperBounds)
  where
  portionsUpperBounds :: NonEmptyArray BigInt'
  portionsUpperBounds = add one <$> portionsLowerBounds

  portionsLowerBounds :: NonEmptyArray BigInt'
  portionsLowerBounds = weights <#> \w -> (bi * w) `div` sum weights

  weights :: NonEmptyArray BigInt'
  weights = unwrap <$> weights'

prop_equipartitionBigInt_trivial :: BigInt' -> IntLeqOne -> Boolean
prop_equipartitionBigInt_trivial bi (IntLeqOne numParts) =
  equipartition bi numParts == NEArray.singleton bi

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/7b0192110fe226f992bca6198b8ee83fa4a37f46/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L124
prop_equipartitionBigInt_length :: BigInt' -> IntGeqOne -> Boolean
prop_equipartitionBigInt_length bi (IntGeqOne numParts) =
  NEArray.length (equipartition bi numParts) == numParts

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/7b0192110fe226f992bca6198b8ee83fa4a37f46/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L134
prop_equipartitionBigInt_sum :: BigInt' -> Int -> Boolean
prop_equipartitionBigInt_sum bi numParts = sum (equipartition bi numParts) == bi

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/7b0192110fe226f992bca6198b8ee83fa4a37f46/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L128
prop_equipartitionBigInt_order :: BigInt' -> Int -> Boolean
prop_equipartitionBigInt_order bi numParts =
  let
    results :: NonEmptyArray BigInt'
    results = equipartition bi numParts
  in
    NEArray.sort results == results

-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/7b0192110fe226f992bca6198b8ee83fa4a37f46/lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs#L112
prop_equipartitionBigInt_fair :: BigInt' -> Int -> Boolean
prop_equipartitionBigInt_fair bi numParts =
  let
    results :: NonEmptyArray BigInt'
    results = equipartition bi numParts
  in
    flip Array.elem [ zero, one ] $
      unwrap (foldMap Max results) - unwrap (foldMap Min results)

newtype BigInt' = BigInt' BigInt

derive newtype instance Eq BigInt'
derive newtype instance Ord BigInt'
derive newtype instance Semiring BigInt'
derive newtype instance Ring BigInt'
derive newtype instance CommutativeRing BigInt'
derive newtype instance EuclideanRing BigInt'
derive newtype instance Equipartition BigInt'
derive newtype instance Partition BigInt'

instance Bounded BigInt' where
  top = BigInt' (BigInt.fromInt top)
  bottom = BigInt' (BigInt.fromInt bottom)

instance Arbitrary BigInt' where
  arbitrary = BigInt' <<< BigInt.fromInt <$> arbitrary

newtype BigIntNeg = BigIntNeg BigInt'

derive instance Newtype BigIntNeg _

instance Arbitrary BigIntNeg where
  arbitrary = BigIntNeg <$> suchThat arbitrary (_ < zero)

newtype BigIntGeqOne = BigIntGeqOne BigInt'

derive instance Newtype BigIntGeqOne _

instance Arbitrary BigIntGeqOne where
  arbitrary = BigIntGeqOne <$> suchThat arbitrary (_ >= one)

newtype IntLeqOne = IntLeqOne Int

derive instance Newtype IntLeqOne _

instance Arbitrary IntLeqOne where
  arbitrary = IntLeqOne <$> suchThat arbitrary (_ <= one)

newtype IntGeqOne = IntGeqOne Int

derive instance Newtype IntGeqOne _

instance Arbitrary IntGeqOne where
  arbitrary = IntGeqOne <$> suchThat arbitrary (_ >= one)
