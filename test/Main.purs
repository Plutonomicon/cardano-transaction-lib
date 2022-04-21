module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ByteArray as ByteArray
import Test.Parser as Parser
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import TestM (TestPlanM)
import Test.Integration as Integration
import Test.Deserialization as Deserialization
import Test.Serialization as Serialization
import Test.Aeson as Aeson
import Test.Transaction as Transaction
import Test.UsedTxOuts as UsedTxOuts
import Test.Data as Data
import Test.Metadata.Seabug as Seabug
import Test.Metadata.Cip25 as Cip25
import Test.Plutus.Address as Plutus.Address
import Test.Utils as Utils

-- we use `mote` here so that we can use effects to build up a test tree, which
-- is then interpreted here in a pure context, mainly due to some painful types
-- in Test.Spec which prohibit effects.
main :: Effect Unit
main = do
  Integration.main
  launchAff_ $ Utils.interpret testPlan

testPlan :: TestPlanM Unit
testPlan = do
  Plutus.Address.suite
  Cip25.suite
  Data.suite
  Aeson.suite
  ByteArray.suite
  Parser.suite
  Serialization.suite
  Serialization.Address.suite
  Serialization.Hash.suite
  Deserialization.suite
  Transaction.suite
  UsedTxOuts.suite
  Seabug.suite
