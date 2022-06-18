module Test.E2E where

import Test.E2E.Wallet as Wallet

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AffInterface as AffInterface
import Test.FinalizeTx as FinalizeTx
import Test.Utils as Utils
import TestM (TestPlanM)
import Prelude
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Mote as Mote
import Test.Spec.Runner as SpecRunner

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret' (SpecRunner.defaultConfig { timeout = pure $ wrap 60_000.0 }) testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM Unit
testPlan = do
--  _ <- Wallet.suite
  testPkh2Pkh
