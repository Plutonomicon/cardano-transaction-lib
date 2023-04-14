module Test.Ctl.Plutip
  ( main
  ) where

import Prelude

import Contract.Test.Plutip (noWallet, testPlutipContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Ctl.Internal.Contract.Monad (wrapQueryM)
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
import Mote.Monad (mapTest)
import Test.Ctl.Plutip.Common (config)
import Test.Ctl.Plutip.Contract as Contract
import Test.Ctl.Plutip.Contract.Assert as Assert
import Test.Ctl.Plutip.Contract.NetworkId as NetworkId
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Ctl.QueryM.AffInterface as QueryM.AffInterface
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run plutip-test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    Utils.interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 110_000.0, exit = true }
      $ group "Plutip" do
          testPlutipContracts config Assert.suite
          Logging.suite
          testStartPlutipCluster
          testPlutipContracts config $ do
            flip mapTest QueryM.AffInterface.suite
              (noWallet <<< wrapQueryM)

            NetworkId.suite
            Contract.suite
          UtxoDistribution.suite

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
