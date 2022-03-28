-- | Tests for `ToData`/`FromData`
module Test.Data (suite) where

import Prelude

import ConstrIndex (class HasConstrIndex, defaultConstrIndex, fromConstr2Index)
import Control.Lazy (fix)
import Data.Array (zip, (..))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep as G
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import FromData (class FromData, fromData, genericFromData)
import Mote (group, test)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (class ToData, genericToData, toData)

suite :: TestPlanM Unit
suite = do
  group "ToData/FromData" $ do
    test "Unit" do
      let
        input = unit
      fromData (toData input) `shouldEqual` Just input
    group "Boolean" do
      let
        inputs = [ true, false ]
      for_ inputs \input -> do
        test (show input) do
          fromData (toData input) `shouldEqual` Just input
    group "BigInt" do
      let
        inputs =
          [ BigInt.fromInt 0
          , BigInt.fromInt 10000
          , BigInt.fromInt (-10000)
          ]
      for_ inputs \input -> do
        test (show input) do
          fromData (toData input) `shouldEqual` Just input
    test "Map #1" do
      let
        input =
          Map.fromFoldable [ unit /\ unit ]
      fromData (toData input) `shouldEqual` Just input
    test "Map #2" do
      let
        input = Map.fromFoldable
          [ Map.fromFoldable [ unit /\ unit ] /\ Map.fromFoldable [ unit /\ unit ] ]
      fromData (toData input) `shouldEqual` Just input
    test "from . to == id" do
      let
        input = E0 D1 true (C2 (MyBigInt (BigInt.fromInt 123)) false)
        --testFn = \input -> fromData (toData input) === Just input
      fromData (toData input) `shouldEqual` Just input

-- | Newtype wrapper to avoid an orphan instance
newtype MyBigInt = MyBigInt BigInt

derive newtype instance ToData MyBigInt
derive newtype instance FromData MyBigInt
derive newtype instance Eq MyBigInt
derive newtype instance Show MyBigInt

instance Arbitrary MyBigInt where
  arbitrary = do
    i <- arbitrary :: Gen Int
    let bi = BigInt.fromInt i
    pure $ MyBigInt bi

-- | Types used to test generic fromData and toData
data TestType = C0 | C1 (Maybe MyBigInt) | C2 MyBigInt Boolean | C3 MyBigInt Boolean Boolean
data TestType1 = D0 TestType MyBigInt (Maybe Boolean) | D1 | D2 TestType
data TestType2 = E0 TestType1 Boolean TestType

derive instance G.Generic TestType _
derive instance G.Generic TestType1 _
derive instance G.Generic TestType2 _
derive instance Eq TestType
derive instance Eq TestType1
derive instance Eq TestType2

instance HasConstrIndex TestType where
  constrIndex = defaultConstrIndex

instance HasConstrIndex TestType1 where
  constrIndex = defaultConstrIndex

instance HasConstrIndex TestType2 where
  constrIndex = defaultConstrIndex

instance FromData TestType1 where
  fromData = genericFromData

instance ToData TestType1 where
  toData = genericToData

instance FromData TestType2 where
  fromData = genericFromData

instance ToData TestType2 where
  toData = genericToData

instance FromData TestType where
  fromData pd = genericFromData pd -- NOTE: https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md

instance ToData TestType where
  toData x = genericToData x -- NOTE: https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md

-- https://stackoverflow.com/questions/51215083/how-to-quickcheck-on-custom-adt-in-purescript
-- https://github.com/purescript/purescript/issues/2975
-- https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md
instance Arbitrary TestType where
  arbitrary = fix $ \arb -> arb

instance Arbitrary TestType2 where
  arbitrary = genericArbitrary

instance Arbitrary TestType1 where
  arbitrary = genericArbitrary

instance Show TestType1 where
  show = genericShow

instance Show TestType2 where
  show = genericShow

instance Show TestType where
  show x = genericShow x

data Day = Mon | Tue | Wed | Thurs | Fri | Sat | Sun

derive instance G.Generic Day _

instance HasConstrIndex Day where
  constrIndex _ = fromConstr2Index (zip [ "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun" ] (0 .. 7))

instance ToData Day where
  toData = genericToData

data AnotherDay = AMon | ATue | AWed | AThurs | AFri | ASat | ASun

derive instance G.Generic AnotherDay _

instance HasConstrIndex AnotherDay where
  constrIndex = defaultConstrIndex

instance ToData AnotherDay where
  toData = genericToData

data Tree a = Node a (Tuple (Tree a) (Tree a)) | Leaf a

derive instance G.Generic (Tree a) _

instance HasConstrIndex (Tree a) where
  constrIndex = defaultConstrIndex

instance (ToData a) => ToData (Tree a) where
  toData x = genericToData x -- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#avoiding-stack-overflow-errors-with-recursive-types
