-- | Tests for `ToData`/`FromData`
module Test.Data (suite) where

import Prelude

import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Traversable (for_, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytes)
import Deserialization.PlutusData as PDD
import FromData (class FromData, fromData)
import Mote (group, test, skip)
import Partial.Unsafe (unsafePartial)
import Serialization (toBytes)
import Serialization.PlutusData as PDS
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (class ToData, toData)
import Types.ByteArray (hexToByteArrayUnsafe)
import Untagged.Union (asOneOf)

suite :: TestPlanM Unit
suite = do
  group "ToData/FromData" do
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
  group "ToData and serialization - binary fixtures" do
    -- How to get binary fixtures:
    --
    -- ```
    -- import PlutusTx
    -- import PlutusTx.IsData
    -- import Codec.Serialise
    -- import Data.ByteString.Lazy as BL
    --
    -- showBinary = toHexDigest . BL.toStrict . serialise . toData
    -- ```
    test "Unit" do
      let
        binaryFixture = "d87980"
        expectedOutput = unit
      fromBytesFromData binaryFixture `shouldEqual` Just expectedOutput
    group "Boolean" do
      let
        fixtures =
          [ false /\ "d87980"
          , true /\ "d87a80"
          ]
      for_ fixtures \(value /\ binaryFixture) -> do
        test (show value) do
          fromBytesFromData binaryFixture `shouldEqual` Just value
    group "Maybe" do
      let
        fixtures =
          [ Nothing /\ "d87a80"
          , Just (BigInt.fromInt 1) /\ "d8799f01ff"
          ]
      for_ fixtures \(value /\ binaryFixture) -> do
        test (show value) do
          fromBytesFromData binaryFixture `shouldEqual` Just value
    group "BigInt" do
      traverse_ (uncurry testBinaryFixture)
        [ BigInt.fromInt (negate 1000) /\ "3903e7"
        , BigInt.fromInt 1000 /\ "1903e8"
        , BigInt.fromInt 1 /\ "01"
        , BigInt.fromInt 0 /\ "00"
        , unsafePartial (fromJust $ BigInt.fromString "999999999999999999999999999999999999999")
            /\ "c25102f050fe938943acc45f65567fffffffff"
        ]
    skip $ group "BigInt - failing (problem with encoding of 0)" do
      traverse_ (uncurry testBinaryFixture)
        [ BigInt.fromInt 0 /\ "00"
        ]

fromBytesFromData :: forall a. FromData a => String -> Maybe a
fromBytesFromData binary = fromData =<< PDD.convertPlutusData =<< fromBytes (hexToByteArrayUnsafe binary)

testBinaryFixture :: forall a. Eq a => Show a => FromData a => ToData a => a -> String -> TestPlanM Unit
testBinaryFixture value binaryFixture = do
  test ("Deserialization: " <> show value) do
    fromBytesFromData binaryFixture `shouldEqual` Just value
  test ("Serialization: " <> show value) do
    map (toBytes <<< asOneOf) (PDS.convertPlutusData (toData value)) `shouldEqual` Just (hexToByteArrayUnsafe binaryFixture)
