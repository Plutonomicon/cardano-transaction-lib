module Test.ProtocolParams
  ( suite
  ) where

import Prelude

import Aeson (decodeAeson)
import Ctl.Internal.Test.Utils as Utils
import Data.Either (Either, isRight)
import Effect.Aff (Aff)
import Mote (group, test)
import QueryM.Ogmios (ProtocolParameters)
import Test.Spec.Assertions (shouldSatisfy)
import TestM (TestPlanM)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  aeson <- Utils.readAeson
    "./fixtures/test/ogmios/currentProtocolParameters.json"
  group "ProtocolParameters parser" $ do
    test "is able to parse ogmios response fixture" $
      (decodeAeson aeson :: Either _ ProtocolParameters) `shouldSatisfy`
        isRight
