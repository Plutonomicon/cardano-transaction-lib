module Test.Integration where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AffInterface as AffInterface
import Test.FinalizeTx as FinalizeTx
import Test.Utils as Utils
import Test.Crypto as Crypto
import TestM (TestPlanM)

main :: Effect Unit
main = launchAff_ $ do
  Utils.interpret testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM Unit
testPlan = do
  FinalizeTx.suite
  Crypto.suite
  AffInterface.suite
