module Test.Ctl.Unit (main, testPlan) where

import Prelude

import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff (Aff, cancelWith, effectCanceler, launchAff)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import Test.Ctl.ApplyArgs as ApplyArgs
import Test.Ctl.Blockfrost.Aeson.Suite as Blockfrost.Aeson
import Test.Ctl.CoinSelection as CoinSelection
import Test.Ctl.CslGc as CslGc
import Test.Ctl.Data as Data
import Test.Ctl.Data.Interval as Ctl.Data.Interval
import Test.Ctl.E2E.Route as E2E.Route
import Test.Ctl.Hashing as Hashing
import Test.Ctl.Internal.Plutus.Time as Plutus.Time
import Test.Ctl.NativeScript as NativeScript
import Test.Ctl.Ogmios.Aeson as Ogmios.Aeson
import Test.Ctl.Ogmios.EvaluateTx as Ogmios.EvaluateTx
import Test.Ctl.Partition as Partition
import Test.Ctl.ProtocolParameters as ProtocolParameters
import Test.Ctl.Serialization as Serialization
import Test.Ctl.Serialization.Hash as Serialization.Hash
import Test.Ctl.Testnet.DistributeFunds as Testnet.DistributeFunds
import Test.Ctl.Types.Interval as Types.Interval
import Test.Ctl.Types.Ipv6 as Ipv6
import Test.Ctl.Types.TokenName as Types.TokenName
import Test.Ctl.Types.Transaction as Types.Transaction
import Test.Ctl.UsedTxOuts as UsedTxOuts
import Test.Ctl.Wallet.Bip32 as Bip32
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Unit`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
      testPlan

testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  Testnet.DistributeFunds.suite
  ApplyArgs.suite
  Ipv6.suite
  NativeScript.suite
  Bip32.suite
  CslGc.suite
  Data.suite
  Hashing.suite
  Partition.suite
  Plutus.Time.suite
  Serialization.suite
  Serialization.Hash.suite
  UsedTxOuts.suite
  Ogmios.Aeson.suite
  Ogmios.EvaluateTx.suite
  ProtocolParameters.suite
  Blockfrost.Aeson.suite
  Types.TokenName.suite
  Types.Transaction.suite
  Ctl.Data.Interval.suite
  flip mapTest Types.Interval.suite \f -> liftEffect $ join $
    f <$> Types.Interval.eraSummariesFixture
      <*> Types.Interval.systemStartFixture
  E2E.Route.suite
  CoinSelection.suite
