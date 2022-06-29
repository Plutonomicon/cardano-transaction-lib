module Test.E2E (main) where

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Utils as Utils
import TestM (TestPlanM)
import Prelude
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Test.Spec.Runner as SpecRunner
import Debug (spy)

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = launchAff_ do
  Utils.interpret' (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM Unit
testPlan = testPkh2Pkh
