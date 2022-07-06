module Test.Unit (main, testPlan) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ByteArray as ByteArray
import Test.Data as Data
import Test.Deserialization as Deserialization
import Test.Hashing as Hashing
import Test.Metadata.Cip25 as Cip25
import Test.Ogmios.EvaluateTx as OgmiosEvaluateTx
import Test.OgmiosDatumCache as OgmiosDatumCache
import Test.Ogmios.Aeson as Ogmios.Aeson
import Test.Parser as Parser
import Test.Plutus.Conversion.Address as Plutus.Conversion.Address
import Test.Plutus.Conversion.Value as Plutus.Conversion.Value
import Test.Plutus.Time as Plutus.Time
import Test.ProtocolParams as ProtocolParams
import Test.Serialization as Serialization
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import Test.Types.TokenName as Types.TokenName
import Test.Transaction as Transaction
import Test.UsedTxOuts as UsedTxOuts
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.Unit`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

testPlan :: TestPlanM Unit
testPlan = do
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
  UsedTxOuts.suite
  OgmiosDatumCache.suite
  Ogmios.Aeson.suite
  OgmiosEvaluateTx.suite
  ProtocolParams.suite
  Types.TokenName.suite
