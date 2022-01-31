module Test.Helpers (suite) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont (lift)
import Data.Argonaut (Json, caseJsonBoolean, caseJsonNull, caseJsonString, parseJson, stringify)
import Data.Argonaut as Json
import Data.Array (length, takeWhile, zip, zipWith)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String (toCodePointArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Helpers (parseJsonStringifyNumbers, jsonTurnNumbersToStrings)
import Mote (group)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)
import Test.ArbitraryJson (ArbBigInt(..), ArbJson(..), stringifyArbJson)
import Test.QuickCheck (class Arbitrary, quickCheck')
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = group "Fixture tests for parseJsonStringifyNumbers" $ do
  parseNumbersTests
  parseStringTests
  parseBoolAndNullTests
  fixtureTests
  quickCheckTests 1000

-- | Test if `parseJsonStringifyNumbers` behaves as if it first replace numbers
-- | by strings in JSON and then parse it using JSON.parse
quickCheckTests :: Int -> TestPlanM Unit
quickCheckTests =
  testFunctionEquivalence
    (stringifyArbJson >>> parseJsonStringifyNumbers)
    (numbersToStrings >>> stringifyArbJson >>> parseJson)
  where
  numbersToStrings :: ArbJson -> ArbJson
  numbersToStrings = case _ of
    (JBigInt (ArbBigInt x)) -> JStr $ BigInt.toString x
    (JNum x) -> JStr $ Json.stringify (Json.fromNumber x)
    (JList l) -> JList $ numbersToStrings <$> l
    (JObject l) -> JObject $ map numbersToStrings <$> l
    x -> x

-- | Construct tests by comparing inputs and expected values read from
-- | file fixtures.
fixtureTests :: TestPlanM Unit
fixtureTests = do
  fixtures <- lift2 zip (readFixtures "input/") (readFixtures "expected/")
  for_ fixtures $ mkTest (uncurry testFixture)
  where
  testFixture :: Tuple FilePath String -> Tuple FilePath String -> Tuple String Boolean
  testFixture (Tuple inputPath input) (Tuple expectedPath expected) =
    let
      output = parseJsonStringifyNumbers input
      result = output == parseJson expected
      err =
        let
          mismatchIx = do
            matches <- liftA1 (zipWith (==) (toCodePointArray expected) <<< toCodePointArray <<< Json.stringify) output
            pure $ length $ takeWhile identity matches
        in
          case mismatchIx of
            Left _ -> "Fixture test failed - parse error in " <> expectedPath <> " or " <> inputPath
            Right ix -> "Fixture test failed - unexpected character at index " <> show ix <> " in file " <> expectedPath
    in
      Tuple err result

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
    Tuple "jsonTurnNumbersToStrings altered null value" (caseJsonNull false (const true) json)

  testBoolean s = testSimpleValue s $ \json ->
    Tuple "jsonTurnNumbersToStrings altered null value" (caseJsonBoolean false (const true) json)

parseNumbersTests :: TestPlanM Unit
parseNumbersTests = do
  testNumber "123123123123123123123100"
  testNumber "100"
  testNumber "0.2"
  testNumber "-10e-20"
  testNumber "20E+20"
  where
  testNumber s = testSimpleValue s $ \json ->
    caseJsonString
      (Tuple ("parseJsonStringifyNumbers did not change number to string when parsing string: " <> stringify json) false)
      (\decoded -> Tuple ("parseJsonStringifyNumbers changed read number: " <> s <> " -> " <> decoded) (decoded == s))
      json

parseStringTests :: TestPlanM Unit
parseStringTests = do
  testString "\"\""
  testString "\"test\""
  testString "\"1231231\""
  testString "\"123\\\"1231\\\"12\""
  testString "\"sth\\\"12sd31\\\"s12\""
  where
  testString s = testSimpleValue s $ \json ->
    caseJsonString
      (Tuple ("parseJsonStringifyNumbers produced no string when parsing string: " <> stringify json) false)
      (\decoded -> Tuple ("parseJsonStringifyNumbers changed read string: " <> s <> " -> " <> decoded) (show decoded == s))
      json

testSimpleValue :: String -> (Json -> Tuple String Boolean) -> TestPlanM Unit
testSimpleValue s jsonCb = uncurry errBool $
  case (parseJson s) of
    Left _ -> Tuple "Invalid json passed to test." false
    Right _ -> case parseJsonStringifyNumbers s of
      Left _ -> Tuple ("Argonaut could not parse jsonTurnNumbersToStrings result: " <> jsonTurnNumbersToStrings s) false
      Right json -> jsonCb json

-- Generic helpers - suitable for exporting

-- | Test if functions produce the same results given same inputs
testFunctionEquivalence :: forall a b. Arbitrary a => Eq b => (a -> b) -> (a -> b) -> Int -> TestPlanM Unit
testFunctionEquivalence f1 f2 n =
  lift $ liftEffect $ quickCheck' n (\x -> f1 x == f2 x)

-- | Make boolean a test
errBool :: String -> Boolean -> TestPlanM Unit
errBool msg b =
  if b then pure unit
  else (liftEffect $ throwException $ error msg)

-- | Make simple test
mkTest :: forall a. (a -> Tuple String Boolean) -> a -> TestPlanM Unit
mkTest doTest inp =
  let
    (Tuple errMsg testRes) = doTest inp
  in
    if testRes then pure unit
    else liftEffect $ throwException $ error errMsg
