module Test.ProtocolParams
  ( suite
  ) where

import Prelude

import Aeson (parseJsonStringToAeson)
import Contract.Aeson (decodeAeson)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either, either, isRight)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import QueryM.Ogmios (ProtocolParameters)
import Test.Spec.Assertions (shouldSatisfy)
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  eAeson <- lift $ parseJsonStringToAeson <$> readTextFile UTF8
    "./fixtures/test/ogmios/currentProtocolParameters.json"
  aeson <- either
    (\e -> liftEffect $ throw ("json parsed incorrectly " <> show e))
    pure
    eAeson
  group "ProtocolParameters parser" $ do
    test "is able to parse ogmios response fixture" $
      (decodeAeson aeson :: Either _ ProtocolParameters) `shouldSatisfy`
        isRight
