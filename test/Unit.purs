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
import Test.Ctl.Blockfrost.ProtocolParameters as Blockfrost.ProtocolParameters
import Test.Ctl.ByteArray as ByteArray
import Test.Ctl.CoinSelection as CoinSelection
import Test.Ctl.CslGc as CslGc
import Test.Ctl.Data as Data
import Test.Ctl.Data.Interval as Ctl.Data.Interval
import Test.Ctl.Deserialization as Deserialization
import Test.Ctl.E2E.Route as E2E.Route
import Test.Ctl.Hashing as Hashing
import Test.Ctl.Internal.Plutus.Conversion.Address as Plutus.Conversion.Address
import Test.Ctl.Internal.Plutus.Conversion.Value as Plutus.Conversion.Value
import Test.Ctl.Internal.Plutus.Time as Plutus.Time
import Test.Ctl.Metadata.Cip25 as Cip25
import Test.Ctl.MustSpendTotal as MustSpendTotal
import Test.Ctl.NativeScript as NativeScript
import Test.Ctl.Ogmios.Address as Ogmios.Address
import Test.Ctl.Ogmios.Aeson as Ogmios.Aeson
import Test.Ctl.Ogmios.EvaluateTx as Ogmios.EvaluateTx
import Test.Ctl.Partition as Partition
import Test.Ctl.ProtocolParams as ProtocolParams
import Test.Ctl.Serialization as Serialization
import Test.Ctl.Serialization.Address as Serialization.Address
import Test.Ctl.Serialization.Hash as Serialization.Hash
import Test.Ctl.Transaction as Transaction
import Test.Ctl.Types.Interval as Types.Interval
import Test.Ctl.Types.Ipv6 as Ipv6
import Test.Ctl.Types.TokenName as Types.TokenName
import Test.Ctl.Types.Transaction as Types.Transaction
import Test.Ctl.UsedTxOuts as UsedTxOuts
import Test.Ctl.Wallet.Bip32 as Bip32
import Test.Ctl.Wallet.Cip30.SignData as Cip30SignData
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
  ApplyArgs.suite
  Ipv6.suite
  NativeScript.suite
  Bip32.suite
  ByteArray.suite
  Cip25.suite
  Cip30SignData.suite
  CslGc.suite
  Data.suite
  Deserialization.suite
  Hashing.suite
  Partition.suite
  Plutus.Conversion.Address.suite
  Plutus.Conversion.Value.suite
  Plutus.Time.suite
  Serialization.suite
  Serialization.Address.suite
  Serialization.Hash.suite
  Transaction.suite
  UsedTxOuts.suite
  Ogmios.Address.suite
  Ogmios.Aeson.suite
  Ogmios.EvaluateTx.suite
  ProtocolParams.suite
  Blockfrost.Aeson.suite
  Blockfrost.ProtocolParameters.suite
  Types.TokenName.suite
  Types.Transaction.suite
  Ctl.Data.Interval.suite
  flip mapTest Types.Interval.suite \f -> liftEffect $ join $
    f <$> Types.Interval.eraSummariesFixture
      <*> Types.Interval.systemStartFixture
  E2E.Route.suite
  MustSpendTotal.suite
  CoinSelection.suite
