module Test.Main where

import Prelude

import Data.Const (Const)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.ByteArray as ByteArrayTest
import Test.Parser as ParseTest
import Test.AffInterface as AffInterface
import Test.Serialization.Address as Serialization.Address
import Test.Serialization.Hash as Serialization.Hash
import TestM (TestPlanM)
import Mote (Plan, foldPlan, planT)
import Test.Deserialization as Deserialization
import Test.Serialization as Serialization
import Test.Aeson as Aeson
import Test.Transaction as Transaction
import Test.UsedTxOuts as UsedTxOuts
import Test.Data as Data
import Test.Metadata.Seabug as Seabug
import Test.FinalizeTx as FinalizeTx
import Test.Plutus.Address as PlutusAddress
import Test.Plutus.Value as PlutusValue
import Test.Metadata.Cip25 as Cip25

-- we use `mote` here so that we can use effects to build up a test tree, which
-- is then interpreted here in a pure context, mainly due to some painful types
-- in Test.Spec which prohibit effects.
main :: Effect Unit
main = do
  launchAff_ $ interpret testPlan

interpret :: TestPlanM Unit -> Aff Unit
interpret spif = do
  plan <- planT $ spif
  let
    spec = go plan
  runSpec [ consoleReporter ] spec
  pure unit
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\{ label, value } -> it label $ liftAff value)
      (\_ -> pure unit)
      (\{ label, value } -> describe label (go $ value))
      sequence_

testPlan :: TestPlanM Unit
testPlan = do
  PlutusAddress.suite
  PlutusValue.suite
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
