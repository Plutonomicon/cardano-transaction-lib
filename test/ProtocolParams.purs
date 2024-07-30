module Test.Ctl.ProtocolParams
  ( suite
  ) where

import Prelude

import Aeson (decodeAeson)
import Ctl.Internal.QueryM.Ogmios (OgmiosProtocolParameters)
import Data.Either (Either, isRight)
import Effect.Aff (Aff)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Test.Ctl.Utils as Utils
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  aeson <- Utils.readAeson
    "./fixtures/test/ogmios/queryLedgerState-protocolParameters-44aa6959a9fdb9d5e92b85678918374e.json"
  group "ProtocolParameters parser" $ do
    test "is able to parse ogmios response fixture" $
      (decodeAeson aeson :: Either _ { result :: OgmiosProtocolParameters })
        `shouldSatisfy`
          isRight
