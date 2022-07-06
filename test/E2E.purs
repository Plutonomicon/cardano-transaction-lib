module Test.E2E (main) where

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Mote (group)
import Prelude (Unit, ($), bind, discard, pure)
import Test.E2E.Browser (TestOptions, parseOptions)
import Test.Examples.Gero (testGero)
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Test.Spec.Runner as SpecRunner
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = launchAff_ $ do
  options <- liftEffect parseOptions
  Utils.interpret'
    (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    (testPlan options)

-- Requires external services listed in README.md
testPlan :: TestOptions -> TestPlanM Unit
testPlan options = group "e2e tests" do
  testGero options
  testPkh2Pkh options

