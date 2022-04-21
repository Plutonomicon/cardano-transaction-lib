module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ByteArray as ByteArrayTest
import Test.Parser as ParseTest
import Test.AffInterface as AffInterface
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import TestM (TestPlanM)
import Test.Deserialization as Deserialization
import Test.Serialization as Serialization
import Test.Aeson as Aeson
import Test.Transaction as Transaction
import Test.UsedTxOuts as UsedTxOuts
import Test.Data as Data
import Test.Metadata.Seabug as Seabug
import Test.FinalizeTx as FinalizeTx
import Test.Plutus.Address as PlutusAddress
import Test.Metadata.Cip25 as Cip25
import Test.Utils as Utils

-- we use `mote` here so that we can use effects to build up a test tree, which
-- is then interpreted here in a pure context, mainly due to some painful types
-- in Test.Spec which prohibit effects.
main :: Effect Unit
main = do
  launchAff_ $ Utils.interpret testPlan

testPlan :: TestPlanM Unit
testPlan = do
  PlutusAddress.suite
  Cip25.suite
  Data.suite
  Aeson.suite
  ByteArrayTest.suite
  ParseTest.suite
  Serialization.suite
  Serialization.Address.suite
  Serialization.Hash.suite
  Deserialization.suite
  Transaction.suite
  UsedTxOuts.suite
  -- requires external servers
  -- see README.md
  FinalizeTx.suite
  AffInterface.suite
  Seabug.suite
