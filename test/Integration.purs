module Test.Ctl.Integration (main, testPlan) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Time (getEraSummaries, getSystemStart)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff (Aff, cancelWith, effectCanceler, launchAff)
import Effect.Class (liftEffect)
import Mote (skip)
import Mote.Monad (mapTest)
import Test.Ctl.BalanceTx.Collateral as Collateral
import Test.Ctl.BalanceTx.Time as BalanceTx.Time
import Test.Ctl.Logging as Logging
import Test.Ctl.PrivateKey as PrivateKey
import Test.Ctl.QueryM.AffInterface as QueryM.AffInterface
import Test.Ctl.Types.Interval as Types.Interval
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Integration`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 450_000.0, exit = true }
      testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  mapTest runQueryM' QueryM.AffInterface.suite
  -- These tests depend on assumptions about testnet history.
  -- We disabled them during transition from `testnet` to `preprod` networks.
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/945
  skip $ flip mapTest Types.Interval.suite \f -> runContract
    testnetConfig { suppressLogs = true }
    do
      eraSummaries <- getEraSummaries
      sysStart <- getSystemStart
      liftEffect $ f eraSummaries sysStart
  Collateral.suite
  PrivateKey.suite
  Logging.suite
  BalanceTx.Time.suite
  where
  runQueryM' =
    runContract (testnetConfig { suppressLogs = true }) <<< wrapQueryM
