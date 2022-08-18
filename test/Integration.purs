module Test.Integration (main, testPlan) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import QueryM (runQueryM)
import QueryM.Config (testnetTraceQueryConfig)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.SystemStart (getSystemStart)
import Test.AffInterface as AffInterface
import Test.PrivateKey as PrivateKey
import Test.Types.Interval as Types.Interval
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.Integration`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  mapTest (runQueryM testnetTraceQueryConfig) AffInterface.suite
  flip mapTest Types.Interval.suite \f -> runQueryM testnetTraceQueryConfig do
    eraSummaries <- getEraSummaries
    sysStart <- getSystemStart
    liftEffect $ f eraSummaries sysStart
  PrivateKey.suite
