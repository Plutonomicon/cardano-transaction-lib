module Test.CTL.Integration (main, testPlan) where

import Prelude

import CTL.Contract.Config (testnetConfig)
import CTL.Contract.Monad (runContract, wrapContract)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import Mote (skip)
import CTL.Internal.QueryM (runQueryM)
import CTL.Internal.QueryM.Config (testnetTraceQueryConfig)
import CTL.Internal.QueryM.EraSummaries (getEraSummaries)
import CTL.Internal.QueryM.SystemStart (getSystemStart)
import Test.CTL.AffInterface as AffInterface
import Test.CTL.Logging as Logging
import Test.CTL.BalanceTx.Collateral as Collateral
import Test.CTL.PrivateKey as PrivateKey
import Test.CTL.Types.Interval as Types.Interval
import Test.CTL.Utils as Utils
import Test.CTL.TestM (TestPlanM)

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
