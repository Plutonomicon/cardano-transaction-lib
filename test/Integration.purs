module Test.Integration (main, testPlan) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (runContract, wrapContract)
import Ctl.Internal.Test.Utils as Utils
import Ctl.Internal.Test.Utils (TestPlanM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote (skip)
import Mote.Monad (mapTest)
import QueryM (runQueryM)
import QueryM.Config (testnetTraceQueryConfig)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.SystemStart (getSystemStart)
import Test.AffInterface as AffInterface
import Test.BalanceTx.Collateral as Collateral
import Test.Logging as Logging
import Test.PrivateKey as PrivateKey
import Test.Types.Interval as Types.Interval

-- Run with `spago test --main Test.Integration`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  mapTest runQueryM' AffInterface.suite
  -- These tests depend on assumptions about testnet history.
  -- We disabled them during transition from `testnet` to `preprod` networks.
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/945
  skip $ flip mapTest Types.Interval.suite \f -> runQueryM
    testnetTraceQueryConfig
    do
      eraSummaries <- getEraSummaries
      sysStart <- getSystemStart
      liftEffect $ f eraSummaries sysStart
  Collateral.suite
  PrivateKey.suite
  Logging.suite
  where
  runQueryM' =
    runContract (testnetConfig { suppressLogs = true }) <<< wrapContract
