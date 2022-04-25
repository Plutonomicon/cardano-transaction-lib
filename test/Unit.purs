module Test.Unit (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Aeson as Aeson
import Test.ByteArray as ByteArray
import Test.Data as Data
import Test.Deserialization as Deserialization
import Test.Metadata.Seabug as Seabug
import Test.Metadata.Cip25 as Cip25
import Test.Parser as Parser
import Test.Plutus.Address as Plutus.Address
import Test.Serialization as Serialization
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import Test.Transaction as Transaction
import Test.UsedTxOuts as UsedTxOuts
import Test.Utils as Utils
import TestM (TestPlanM)

main :: Effect Unit
main = launchAff_ $ Utils.interpret testPlan

testPlan :: TestPlanM Unit
testPlan = do
  Aeson.suite
  ByteArray.suite
  Cip25.suite
  Data.suite
  Deserialization.suite
  Parser.suite
  Plutus.Address.suite
  Seabug.suite
  Serialization.suite
  Serialization.Address.suite
  Serialization.Hash.suite
  Transaction.suite
  UsedTxOuts.suite
