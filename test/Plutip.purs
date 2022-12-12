module Test.Ctl.Plutip
  ( main
  ) where

import Prelude

import Contract.Test.Plutip (testPlutipContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Ctl.Internal.Plutip.Server
  ( checkPlutipServer
  , startPlutipCluster
  , startPlutipServer
  , stopChildProcessWithPort
  , stopPlutipCluster
  )
import Ctl.Internal.Plutip.Types (StopClusterResponse(StopClusterSuccess))
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.TestPlanM as Utils
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Milliseconds(Milliseconds)
  , bracket
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group, test)
import Test.Ctl.Plutip.Common (config)
import Test.Ctl.Plutip.Contract as Contract
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)
 
-- Run with `npm run plutip-test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    Utils.interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
      $ group "Plutip" do
          Logging.suite
          testStartPlutipCluster
          testPlutipContracts config Contract.suite
          UtxoDistribution.suite

testStartPlutipCluster :: TestPlanM (Aff Unit) Unit
testStartPlutipCluster = group "Server" do
  test "startPlutipCluster / stopPlutipCluster" do
    bracket (startPlutipServer config)
      (stopChildProcessWithPort config.port) $ const do
      checkPlutipServer config
      _startRes <- startPlutipCluster config [[]]
      stopRes <- stopPlutipCluster config
      stopRes `shouldSatisfy` case _ of
        StopClusterSuccess -> true
        _ -> false
