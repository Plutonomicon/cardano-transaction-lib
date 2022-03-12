module Test.Aeson where

import Prelude

import Aeson
  ( Aeson
  , AesonCases
  , caseAeson
  , constAesonCases
  , caseAesonString
  , caseAesonNull
  , caseAesonBoolean
  , decodeAeson
  , decodeJsonString
  , getField
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , toObject
  , toStringifiedNumbersJson
  , stringifyAeson
  , getNumberIndex
  )
import Control.Apply (lift2)
import Control.Monad.Cont (lift)
import Data.Argonaut (encodeJson, parseJson)
import Data.Argonaut as Json
import Data.Array (head, length, (!!), zip)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), hush)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, isJust)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Test.ArbitraryJson (stringifyArbJson)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck (quickCheck', (<?>))
import Test.Utils (assertTrue)
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
      Json.stringify (toStringifiedNumbersJson asn) `shouldEqual` expected

    test "jsonToAeson" $ liftEffect do
      let
        jsn = unsafePartial $ fromRight $ parseJson
          "{\"a\":10,\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
        expected = "{\"a\":\"10\",\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
      (jsonToAeson jsn # toStringifiedNumbersJson # Json.stringify) `shouldEqual` expected

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

  group "Fixture tests for parseJsonStringifyNumbers" $ do
    parseNumbersTests
    parseStringTests
    parseBoolAndNullTests
    fixtureTests

  group "Arbitrary Aeson" do
    testArbitraryAeson

-- | This function reads from `./fixtures/` folder.
-- | `expected/*` contains JSONs corresponding to `Aeson` type (with number
-- | index) as returned by `parseJsonExtractingIntegers`.
-- | Here the number index in fixtures is compared with the parsed one.
fixtureTests :: TestPlanM Unit
fixtureTests = do
  fixtures <- lift2 zip (readFixtures "input/") (readFixtures "expected/")
  for_ fixtures $ mkTest (uncurry testFixture)
  where
  testFixture :: Tuple FilePath String -> Tuple FilePath String -> Tuple String Boolean
  testFixture (Tuple inputPath input) (Tuple _expectedPath expected) =
    let
      mkError msg = Tuple (msg <> ": " <> show inputPath) false
    in
      case parseJsonStringToAeson input /\ Json.jsonParser expected of
        Right aeson /\ Right json ->
          case Json.caseJsonArray Nothing (\arr -> arr !! 1) json of
            Nothing -> mkError "Failed to decode expected json"
            Just (res :: Json.Json) ->
              case Json.decodeJson res :: Either _ (Array String) of
                Left _ -> mkError "Unable to decode NumberIndex"
                Right numberStrings ->
                  if getNumberIndex aeson == numberStrings then Tuple "NumberIndex is correct" true -- success
                  else mkError "NumberIndex does not match fixture"
        Left err /\ _ -> mkError $ "Failed to parse input JSON: " <> show err
        _ /\ Left err -> mkError $ "Failed to parse expected JSON: " <> show err

  readFixtures :: FilePath -> TestPlanM (Array (Tuple FilePath String))
  readFixtures dirn = lift $
    let
      d = (fixtureDir <> dirn)
      readTestFile fp = Tuple fp <$> readTextFile UTF8 fp
    in
      readdir d >>= traverse (readTestFile <<< (<>) d)

  fixtureDir = "./fixtures/test/parsing/json_stringify_numbers/"

parseBoolAndNullTests :: TestPlanM Unit
parseBoolAndNullTests = do
  testNull
  testBoolean "false"
  testBoolean "true"
  where
  testNull = testSimpleValue "null" $ \json ->
    Tuple "jsonTurnNumbersToStrings altered null value" (caseAesonNull false (const true) json)

  testBoolean s = testSimpleValue s $ \json ->
    Tuple "jsonTurnNumbersToStrings altered null value" (caseAesonBoolean false (const true) json)

parseNumbersTests :: TestPlanM Unit
parseNumbersTests = do
  testNumber "123123123123123123123100"
  testNumber "100"
  testNumber "0.2"
  testNumber "-10e-20"
  testNumber "20E+20"
  where
  testNumber s = testSimpleValue s $ \aeson -> do
    if length (getNumberIndex aeson) == 0 then
      Tuple ("parseJsonStringifyNumbers did not change number to string when parsing string: " <> stringifyAeson aeson) false
    else Tuple
      ("parseJsonStringifyNumbers changed read number: " <> s <> " -> " <> s)
      (stringifyAeson aeson == s)

parseStringTests :: TestPlanM Unit
parseStringTests = do
  testString "\"\""
  testString "\"test\""
  testString "\"1231231\""
  testString "\"123\\\"1231\\\"12\""
  testString "\"sth\\\"12sd31\\\"s12\""
  where
  testString s = testSimpleValue s $ \aeson ->
    caseAesonString
      (Tuple ("parseJsonStringifyNumbers produced no string when parsing string: " <> stringifyAeson aeson) false)
      (\decoded -> Tuple ("parseJsonStringifyNumbers changed read string: " <> s <> " -> " <> decoded) (show decoded == s))
      aeson

caseMaybeAeson
  :: forall b a
   . (AesonCases (Maybe a) -> AesonCases (Maybe b))
  -> Aeson
  -> Maybe b
caseMaybeAeson upd = caseAeson (constAesonCases Nothing # upd)

fromRight :: forall (a :: Type) (e :: Type). Partial => Either e a -> a
fromRight (Right x) = x

-- | Make simple test
mkTest :: forall a. (a -> Tuple String Boolean) -> a -> TestPlanM Unit
mkTest doTest inp =
  let
    Tuple errMsg testRes = doTest inp
  in
    if testRes then pure unit
    else liftEffect $ throwException $ error errMsg

testSimpleValue :: String -> (Aeson -> Tuple String Boolean) -> TestPlanM Unit
testSimpleValue s jsonCb = uncurry assertTrue $
  case (parseJson s) of
    Left _ -> Tuple "Invalid json passed to test." false
    Right _ -> case parseJsonStringToAeson s of
      Left _ -> Tuple ("Argonaut could not parse jsonTurnNumbersToStrings result: " <> s) false
      Right json -> jsonCb json

testArbitraryAeson :: TestPlanM Unit
testArbitraryAeson = liftEffect $ quickCheck' 3000 \arbJson ->
  let
    jsonString = stringifyArbJson arbJson
    res = do
      aeson1 <- hush $ parseJsonStringToAeson jsonString
      aeson2 <- hush $ parseJsonStringToAeson $ stringifyAeson aeson1
      pure $ aeson1 /\ aeson2
  in
    fromMaybe false (res <#> uncurry eq) <?>
      "Test failed for input " <> show (isJust res) <> " - " <> jsonString
