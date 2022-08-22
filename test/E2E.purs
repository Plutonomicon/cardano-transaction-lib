module Test.E2E (main) where

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote (group)
import Prelude (Unit, ($), bind, discard, pure)
import Contract.Test.E2E (TestOptions, parseOptions)
import Test.E2E.Examples.Gero as Gero
import Test.E2E.Examples.Pkh2PkhGero as Pkh2PkhGero
import Test.E2E.Examples.Pkh2Pkh as Pkh2Pkh
import Test.E2E.Examples.AlwaysMints as AlwaysMints
import Test.E2E.Examples.AlwaysSucceeds as AlwaysSucceeds
import Test.E2E.Examples.Datums as Datums
import Test.E2E.Examples.MintsMultipleTokens as MintsMultipleTokens
import Test.E2E.Examples.SignMultiple as SignMultiple
import Test.Spec.Runner as SpecRunner
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = launchAff_ $ do
  options <- liftEffect parseOptions
  Utils.interpretWithConfig
    (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    (testPlan options)

-- Requires external services listed in README.md
testPlan :: TestOptions -> TestPlanM (Aff Unit) Unit
testPlan options = group "e2e tests" do
  Gero.runExample options
  Pkh2PkhGero.runExample options
  Pkh2Pkh.runExample options
  AlwaysMints.runExample options
  AlwaysSucceeds.runExample options
  Datums.runExample options
  MintsMultipleTokens.runExample options
  SignMultiple.runExample options
