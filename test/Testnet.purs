module Test.Ctl.Testnet
  ( main
  ) where

import Prelude

import Contract.Test (noWallet)
import Contract.Test.Testnet
  ( defaultTestnetConfig
  , runTestnetTestPlan
  , testTestnetContracts
  , testnetConfigWithMaxExUnits
  )
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Ctl.Internal.Contract.Monad (wrapKupmiosM)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds), fromDuration)
import Effect (Effect)
import Effect.Aff (cancelWith, effectCanceler, launchAff)
import Mote (group)
import Mote.Monad (mapTest)
import Mote.TestPlanM as Utils
import Test.Ctl.BalanceTx.ChangeGeneration as ChangeGeneration
import Test.Ctl.KupmiosM.AffInterface as KupmiosM.AffInterface
import Test.Ctl.Testnet.ClusterParameters (runTest) as ClusterParameters
import Test.Ctl.Testnet.Contract as Contract
import Test.Ctl.Testnet.Contract.Assert as Assert
import Test.Ctl.Testnet.Contract.Mnemonics as Mnemonics
import Test.Ctl.Testnet.Contract.OgmiosMempool as OgmiosMempool
import Test.Ctl.Testnet.ExUnits as ExUnits
import Test.Ctl.Testnet.Gov as Gov
import Test.Ctl.Testnet.Logging as Logging
import Test.Ctl.Testnet.SameWallets as SameWallets
import Test.Ctl.Testnet.UtxoDistribution as UtxoDistribution
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run testnet-test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  let config = defaultTestnetConfig
  flip cancelWith (effectCanceler (exitCode 1)) do
    Utils.interpretWithConfig
      defaultConfig
        { timeout = Just $ fromDuration $ Seconds 70.0, exit = true }
      $ group "cardano-testnet" do
          testTestnetContracts config Mnemonics.suite
          group "ExUnits - normal limits" do
            testTestnetContracts config $ ExUnits.mkFailingSuite 8000
            testTestnetContracts config $ ExUnits.mkSuite 2550
          group "ExUnits - relaxed limits" do
            testTestnetContracts testnetConfigWithMaxExUnits $ ExUnits.mkSuite
              10_000
          testTestnetContracts config Assert.suite
          Logging.suite
          testTestnetContracts config $ do
            flip mapTest KupmiosM.AffInterface.suite
              (noWallet <<< wrapKupmiosM)
            ChangeGeneration.suite
            Contract.suite
            Gov.suite
          UtxoDistribution.suite
          testTestnetContracts config OgmiosMempool.suite
          runTestnetTestPlan config SameWallets.suite
          ClusterParameters.runTest
