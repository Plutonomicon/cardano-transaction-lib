module Test.ProtocolParams
  ( suite
  ) where

import Prelude

import Contract.Aeson (decodeAeson)
import Data.Either (Either, isRight)
import Mote (group, test)
import QueryM.Ogmios (ProtocolParameters)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Utils as Utils
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  aeson <- Utils.readAeson
    "./fixtures/test/ogmios/currentProtocolParameters.json"
  group "ProtocolParameters parser" $ do
    test "is able to parse ogmios response fixture" $
      (decodeAeson aeson :: Either _ ProtocolParameters) `shouldSatisfy`
        isRight
