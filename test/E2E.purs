module Test.E2E (main) where

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Mote (group)
import Prelude (Unit, ($), bind, discard, pure)
import Test.E2E.Browser (TestOptions, parseOptions)
import Test.Examples.Gero as Gero
import Test.Examples.Pkh2PkhGero as Pkh2PkhGero
import Test.Examples.Pkh2Pkh as Pkh2Pkh
import Test.Examples.AlwaysMints as AlwaysMints
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
  Gero.runExample options
  Pkh2PkhGero.runExample options
  Pkh2Pkh.runExample options
  AlwaysMints.runExample options

