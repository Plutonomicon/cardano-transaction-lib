module Ctl.Test.Unit (main, testPlan) where

import Prelude

import Ctl.Internal.Test.Utils as Utils
import Ctl.Internal.Test.Utils (TestPlanM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote.Monad (mapTest)
import Test.Base64 as Base64
import Test.ByteArray as ByteArray
import Test.Data as Data
import Test.Deserialization as Deserialization
import Test.Hashing as Hashing
import Test.Metadata.Cip25 as Cip25
import Test.NativeScript as NativeScript
import Test.Ogmios.Address as Ogmios.Address
import Test.Ogmios.Aeson as Ogmios.Aeson
import Test.Ogmios.EvaluateTx as Ogmios.EvaluateTx
import Test.OgmiosDatumCache as OgmiosDatumCache
import Test.Parser as Parser
import Test.Plutus.Conversion.Address as Plutus.Conversion.Address
import Test.Plutus.Conversion.Value as Plutus.Conversion.Value
import Test.Plutus.Time as Plutus.Time
import Test.ProtocolParams as ProtocolParams
import Test.Serialization as Serialization
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import Test.Transaction as Transaction
import Test.TxOutput as TxOutput
import Test.Types.Interval as Types.Interval
import Test.Types.TokenName as Types.TokenName
import Test.UsedTxOuts as UsedTxOuts

-- Run with `spago test --main Ctl.Test.Unit`
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
