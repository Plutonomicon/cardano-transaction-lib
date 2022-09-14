module Test.CTL.Unit (main, testPlan) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import Test.CTL.Base64 as Base64
import Test.CTL.ByteArray as ByteArray
import Test.CTL.Data as Data
import Test.CTL.Deserialization as Deserialization
import Test.CTL.Hashing as Hashing
import Test.CTL.Metadata.Cip25 as Cip25
import Test.CTL.NativeScript as NativeScript
import Test.CTL.Ogmios.Address as Ogmios.Address
import Test.CTL.Ogmios.Aeson as Ogmios.Aeson
import Test.CTL.Ogmios.EvaluateTx as Ogmios.EvaluateTx
import Test.CTL.OgmiosDatumCache as OgmiosDatumCache
import Test.CTL.Parser as Parser
import Test.CTL.Plutus.Conversion.Address as Plutus.Conversion.Address
import Test.CTL.Plutus.Conversion.Value as Plutus.Conversion.Value
import Test.CTL.Plutus.Time as Plutus.Time
import Test.CTL.ProtocolParams as ProtocolParams
import Test.CTL.Serialization as Serialization
import Test.CTL.Serialization.Address as Serialization.Address
import Test.CTL.Serialization.Hash as Serialization.Hash
import Test.CTL.TestM (TestPlanM)
import Test.CTL.Transaction as Transaction
import Test.CTL.TxOutput as TxOutput
import Test.CTL.Types.Interval as Types.Interval
import Test.CTL.Types.TokenName as Types.TokenName
import Test.CTL.UsedTxOuts as UsedTxOuts
import Test.CTL.Utils as Utils

-- Run with `spago test --main Test.CTL.Unit`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  NativeScript.suite
  Base64.suite
  ByteArray.suite
  Cip25.suite
  Data.suite
  Deserialization.suite
  Hashing.suite
  Parser.suite
  Plutus.Conversion.Address.suite
  Plutus.Conversion.Value.suite
  Plutus.Time.suite
  Serialization.suite
  Serialization.Address.suite
  Serialization.Hash.suite
  Transaction.suite
  TxOutput.suite
  UsedTxOuts.suite
  OgmiosDatumCache.suite
  Ogmios.Address.suite
  Ogmios.Aeson.suite
  Ogmios.EvaluateTx.suite
  ProtocolParams.suite
  Types.TokenName.suite
  flip mapTest Types.Interval.suite \f -> liftEffect $ join $
    f <$> Types.Interval.eraSummariesFixture
      <*> Types.Interval.systemStartFixture
