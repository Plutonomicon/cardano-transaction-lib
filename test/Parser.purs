module Test.Parser where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Either (Either, either, isRight)
import Data.Medea (validate)
import Data.Medea.Loader (LoaderError, loadSchemaFromFile)
import Data.Medea.Schema (Schema)
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrowException)
import Node.FS.Aff (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Spec.Assertions (shouldSatisfy, shouldNotSatisfy)
import TestM (TestPlanM, ValidationM, runValidationM)
import Mote (group, test)
import Types.JsonWsp (JsonWspResponse, UtxoQR, parseJsonWspResponse)

suite :: TestPlanM Unit
suite = do
  eJson <- lift $ Json.parseJson <$> readTextFile UTF8 "./fixtures/test/parsing/JsonWsp/UtxoQueryResponse.json"
  -- we use unsafeThrowException because we're technically in Aff here and that's part of the contract.
  json <- either
    (\e -> unsafeThrowException $ error ("json parsed incorrectly " <> show e))
    pure
    eJson
  let (stringArray :: Array String) = Json.caseJsonArray [] convertJsonArray json
  let (jsonArray :: Array Json.Json) = Json.caseJsonArray [] identity json
  schema <- lift $ getSchema "./fixtures/schemata/JsonWsp/UtxoQueryResponse.medea"
  group "Parser tests" $ do
    group "Schemata parse tests" $ do
      test "fixture array should not be empty" $
        stringArray `shouldNotSatisfy` Array.null
      test "fixtures match schema - utxoQueryResponse" $
        -- TODO: add a helper function or something so that the error displays the index it occured on, logs out the offending JSON string from the array.
        (runValidationM $ validateJsonArray schema stringArray) `shouldSatisfy` isRight
    group "Type parsing" $ do
      test "fixtures parse correctly - UtxoQueryResponse" $
        (traverseJsonWsps jsonArray) `shouldSatisfy` isRight

traverseJsonWsps :: Array Json.Json -> Either Json.JsonDecodeError (Array (JsonWspResponse UtxoQR))
traverseJsonWsps arr = traverse parseJsonWspResponse arr

convertJsonArray :: Array Json.Json -> Array String
convertJsonArray arr = map Json.stringify arr

getSchema :: String -> Aff Schema
getSchema file = do
  (eSchema :: Either LoaderError Schema) <- runExceptT $
    (loadSchemaFromFile file :: ExceptT LoaderError Aff Schema)
  either (throwError <<< error <<< show) pure eSchema

validateJsonArray :: Schema -> Array String -> ValidationM Unit
validateJsonArray scm arr = traverse_ (validate scm) arr

