module Test.CTL.ProtocolParams
  ( suite
  ) where

import Prelude

import Aeson (decodeAeson)
import CTL.Internal.QueryM.Ogmios (ProtocolParameters)
import Data.Either (Either, isRight)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.CTL.TestM (TestPlanM)
import Test.CTL.Utils as Utils
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  aeson <- Utils.readAeson
    "./fixtures/test/ogmios/currentProtocolParameters.json"
  group "ProtocolParameters parser" $ do
    test "is able to parse ogmios response fixture" $
      (decodeAeson aeson :: Either _ ProtocolParameters) `shouldSatisfy`
        isRight
