module Test.Aeson where

import Prelude

import Aeson (decodeAesonString)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.PlutusData (PlutusData(..))

suite :: TestPlanM Unit
suite = do
  group "Aeson decoder" do
    test "Integer" $ liftEffect do
      let
        expected =
          Integer $ unsafePartial $ fromJust $ BigInt.fromString "999999999999999999999999"
      decodeAesonString "999999999999999999999999" `shouldEqual` Right expected
    test "Bytes" $ liftEffect do
      let
        expected =
          Bytes $ hexToByteArrayUnsafe "00FFAA"
      decodeAesonString "\"00FFAA\"" `shouldEqual` Right expected
    test "List" $ liftEffect do
      let
        expected =
          List [ Bytes $ hexToByteArrayUnsafe "00FFAA", Integer $ BigInt.fromInt 1 ]
      decodeAesonString "[\"00FFAA\", 1]" `shouldEqual` Right expected
    test "Map #1" $ liftEffect do
      let
        expected =
          Map (Map.fromFoldable [ Bytes (hexToByteArrayUnsafe "00FFAA") /\ Integer (BigInt.fromInt 1) ])
      decodeAesonString "{\"map\": [ { \"key\": \"00FFAA\", \"value\": 1 } ] }" `shouldEqual` Right expected
    test "Map #2" $ liftEffect do
      let
        input =
          "{\"map\": \
          \ [ { \"key\": \"00FFAA\", \"value\": 1 },\
          \   { \"key\": \"AAAA\", \"value\": 200 } ] }"
        expected = Map $ Map.fromFoldable
          [ Bytes (hexToByteArrayUnsafe "00FFAA") /\ Integer (BigInt.fromInt 1)
          , Bytes (hexToByteArrayUnsafe "AAAA") /\ Integer (BigInt.fromInt 200)
          ]
      decodeAesonString input `shouldEqual` Right expected
    test "Constr" $ liftEffect do
      let
        input =
          "{\"constr\": 1, \"fields\": [ 1, 2, 3 ] }"
        expected = Constr (BigInt.fromInt 1)
          [ Integer $ BigInt.fromInt 1
          , Integer $ BigInt.fromInt 2
          , Integer $ BigInt.fromInt 3
          ]
      decodeAesonString input `shouldEqual` Right expected
