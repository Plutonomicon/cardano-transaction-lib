module Test.Ctl.Equipartition (suite) where

import Prelude

import Ctl.Internal.Equipartition (class Equipartition, equipartition)
import Data.Array (elem) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (length, singleton, sort) as NEArray
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Foldable (foldMap, sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.TestM (TestPlanM)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "Equipartition" do
    group "equipartitionBigInt" do
      test "returns an arr containing only the input value if `numParts` leq 1"
        (quickCheck prop_equipartitionBigInt_trivial)

      test "returns an arr containing `numParts` elements if `numParts` geq 1"
        (quickCheck prop_equipartitionBigInt_length)

      test "returns an arr with the sum of elements equal to the input value"
        (quickCheck prop_equipartitionBigInt_sum)

      test "returns an arr sorted in ascending order"
        (quickCheck prop_equipartitionBigInt_order)

      test "returns an arr containing fairly equipartitioned portions"
        (quickCheck prop_equipartitionBigInt_fair)

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
prop_equipartitionBigInt_sum bi numParts =
  sum (equipartition bi numParts) == bi

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
derive newtype instance Equipartition BigInt'

instance Bounded BigInt' where
  top = BigInt' (BigInt.fromInt top)
  bottom = BigInt' (BigInt.fromInt bottom)

instance Arbitrary BigInt' where
  arbitrary = BigInt' <<< BigInt.fromInt <$> arbitrary

newtype IntLeqOne = IntLeqOne Int

derive instance Newtype IntLeqOne _

instance Arbitrary IntLeqOne where
  arbitrary = IntLeqOne <$> suchThat arbitrary (_ <= one)

newtype IntGeqOne = IntGeqOne Int

derive instance Newtype IntGeqOne _

instance Arbitrary IntGeqOne where
  arbitrary = IntGeqOne <$> suchThat arbitrary (_ >= one)

