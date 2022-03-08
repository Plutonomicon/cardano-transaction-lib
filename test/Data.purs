-- | Tests for `ToData`/`FromData`
module Test.Data (suite) where

import Prelude

import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import FromData (fromData)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (toData)

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
