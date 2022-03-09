module Test.Aeson where

import Prelude

import Aeson
  ( Aeson
  , AesonCases
  , caseAeson
  , constAesonCases
  , decodeAeson
  , decodeJsonString
  , getField
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , toObject
  , toStringifiedNumbersJson
  )
import Data.Argonaut (encodeJson, parseJson, stringify)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Either (Either(Right))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.PlutusData
  ( PlutusData
      ( Integer
      , Bytes
      , List
      , Map
      , Constr
      )
  )

suite :: TestPlanM Unit
suite = do
  group "Aeson decoder" do
    test "Integer" $ liftEffect do
      let
        expected =
          Integer $ unsafePartial $ fromJust $ BigInt.fromString "999999999999999999999999"
      decodeJsonString "999999999999999999999999" `shouldEqual` Right expected
    test "Bytes" $ liftEffect do
      let
        expected =
          Bytes $ hexToByteArrayUnsafe "00FFAA"
      decodeJsonString "\"00FFAA\"" `shouldEqual` Right expected
    test "List" $ liftEffect do
      let
        expected =
          List [ Bytes $ hexToByteArrayUnsafe "00FFAA", Integer $ BigInt.fromInt 1 ]
      decodeJsonString "[\"00FFAA\", 1]" `shouldEqual` Right expected
    test "Map #1" $ liftEffect do
      let
        expected =
          Map (Map.fromFoldable [ Bytes (hexToByteArrayUnsafe "00FFAA") /\ Integer (BigInt.fromInt 1) ])
      decodeJsonString "{\"map\": [ { \"key\": \"00FFAA\", \"value\": 1 } ] }" `shouldEqual` Right expected
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
      decodeJsonString input `shouldEqual` Right expected
    test "Constr" $ liftEffect do
      let
        input =
          "{\"constr\": 1, \"fields\": [ 1, 2, 3 ] }"
        expected = Constr (BigInt.fromInt 1)
          [ Integer $ BigInt.fromInt 1
          , Integer $ BigInt.fromInt 2
          , Integer $ BigInt.fromInt 3
          ]
      decodeJsonString input `shouldEqual` Right expected

    test "Record" $ liftEffect do
      let
        expected = { a: 10 }
      decodeJsonString "{\"a\": 10}" `shouldEqual` Right expected

  group "Object field accessing" do
    let
      asn = unsafePartial $ fromRight $ parseJsonStringToAeson
        "{\"a\": 10, \"b\":[{\"b1\":\"valb\"}], \"c\":{\"c1\": \"valc\"}}"
      asnObj = unsafePartial $ fromJust $ toObject $ asn
    test "getField" $ liftEffect do
      getField asnObj "a" `shouldEqual` Right 10
      getField asnObj "b" `shouldEqual` Right ([ { b1: "valb" } ])

    test "getNestedAeson" $ liftEffect do
      (getNestedAeson asn [ "c", "c1" ] >>= decodeAeson) `shouldEqual` (Right "valc")

  group "Json <-> Aeson" do
    test "toStringifiedNumbersJson" $ liftEffect do
      let
        asn = unsafePartial $ fromRight $ parseJsonStringToAeson
          "{\"a\":10,\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
        expected = "{\"a\":\"10\",\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
      (toStringifiedNumbersJson asn # stringify) `shouldEqual` expected

    test "jsonToAeson" $ liftEffect do
      let
        jsn = unsafePartial $ fromRight $ parseJson
          "{\"a\":10,\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
        expected = "{\"a\":\"10\",\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
      (jsonToAeson jsn # toStringifiedNumbersJson # stringify) `shouldEqual` expected

  group "caseAeson" do
    test "caseObject" $ liftEffect do
      let asn = jsonToAeson $ encodeJson { a: 10 }
      (caseMaybeAeson _ { caseObject = Just <<< flip getField "a" }) asn `shouldEqual` (Just $ Right 10)
    test "caseArray" $ liftEffect do
      let asn = jsonToAeson $ encodeJson [ 10 ]
      (caseMaybeAeson _ { caseArray = map decodeAeson <<< head }) asn `shouldEqual` (Just $ Right 10)
    test "caseNumber" $ liftEffect do
      let asn = jsonToAeson $ encodeJson 20222202
      (caseMaybeAeson _ { caseNumber = Just }) asn `shouldEqual` (Just "20222202")

caseMaybeAeson
  :: forall b a
   . (AesonCases (Maybe a) -> AesonCases (Maybe b))
  -> Aeson
  -> Maybe b
caseMaybeAeson upd = caseAeson (constAesonCases Nothing # upd)

fromRight :: forall (a :: Type) (e :: Type). Partial => Either e a -> a
fromRight (Right x) = x
