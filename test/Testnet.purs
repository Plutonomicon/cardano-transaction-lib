module Test.Ctl.Testnet
  ( main
  ) where

import Prelude

import Contract.Test.Testnet (defaultTestnetConfig, testTestnetContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group)
import Mote.TestPlanM as Utils
import Test.Ctl.Testnet.Contract.OgmiosMempool as OgmiosMempool
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run testnet-test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  let config = defaultTestnetConfig
  flip cancelWith (effectCanceler (exitCode 1)) do
    Utils.interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
      $ group "cardano-testnet" do
          testTestnetContracts config OgmiosMempool.suite
-- FIXME: ClusterParameters.runTest

{-
configWithMaxExUnits :: PlutipConfig
configWithMaxExUnits = config
  { clusterConfig = config.clusterConfig { raiseExUnitsToMax = true } }

testStartPlutipCluster :: TestPlanM (Aff Unit) Unit
testStartPlutipCluster = group "Server" do
  test "startPlutipCluster / stopPlutipCluster" do
    bracket (startPlutipServer config)
      (stopChildProcessWithPort config.port) $ const do
      checkPlutipServer config
      _startRes <- startPlutipCluster config [ [] ]
      stopRes <- stopPlutipCluster config
      stopRes `shouldSatisfy` case _ of
        StopClusterSuccess -> true
        _ -> false
        -}
