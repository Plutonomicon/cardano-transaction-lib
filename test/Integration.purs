module Test.Integration (main, testPlan) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AffInterface as AffInterface
import Test.FinalizeTx as FinalizeTx
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.Integration`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM Unit
testPlan = do
  AffInterface.suite
  FinalizeTx.suite
-- FIXME: MultipleRedeemers.suite
