module Test.Ctl.Plutip
  ( main
  ) where

import Prelude

import Contract.Test.Plutip (noWallet)
import Contract.Test.Testnet (defaultTestnetConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Testnet.Contract (runTestnetTestPlan, testTestnetContracts)
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
import Mote.Monad (mapTest)
import Mote.TestPlanM as Utils
import Test.Ctl.BalanceTx.ChangeGeneration as ChangeGeneration
import Test.Ctl.Plutip.Contract as Contract
import Test.Ctl.Plutip.Contract.Assert as Assert
import Test.Ctl.Plutip.Contract.Mnemonics as Mnemonics
import Test.Ctl.Plutip.Contract.OgmiosMempool as OgmiosMempool
import Test.Ctl.Plutip.ExUnits as ExUnits
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.SameWallets as SameWallets
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Ctl.QueryM.AffInterface as QueryM.AffInterface
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run plutip-test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  let config = defaultTestnetConfig
  flip cancelWith (effectCanceler (exitCode 1)) do
    Utils.interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
      $ group "cardano-testnet" do
          testTestnetContracts config Mnemonics.suite
          group "ExUnits - normal limits" do
            testTestnetContracts config $ ExUnits.mkFailingSuite 8000
            testTestnetContracts config $ ExUnits.mkSuite 2550
          -- FIXME: group "ExUnits - relaxed limits" do
          --   testTestnetContracts configWithMaxExUnits $ ExUnits.mkSuite 3000
          testTestnetContracts config Assert.suite
          Logging.suite
          -- FIXME: testStartPlutipCluster
          testTestnetContracts config $ do
            flip mapTest QueryM.AffInterface.suite
              (noWallet <<< wrapQueryM)
            ChangeGeneration.suite
            Contract.suite
          UtxoDistribution.suite
          testTestnetContracts config OgmiosMempool.suite
          runTestnetTestPlan config SameWallets.suite
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
