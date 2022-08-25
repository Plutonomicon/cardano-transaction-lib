module Test.Integration (main, testPlan) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (runContract, wrapContract)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.SystemStart (getSystemStart)
import Test.AffInterface as AffInterface
import Test.Logging as Logging
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
  mapTest runQueryM' AffInterface.suite
  Types.Interval.suite `flip mapTest` \f -> runQueryM' do
    eraSummaries <- getEraSummaries
    sysStart <- getSystemStart
    liftEffect $ f eraSummaries sysStart
  PrivateKey.suite
  Logging.suite
  where
  runQueryM' =
    runContract (testnetConfig { suppressLogs = true }) <<< wrapContract
