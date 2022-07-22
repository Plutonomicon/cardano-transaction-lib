module Contract.Test.E2E (main) where

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Mote (group)
import Prelude (Unit, ($), bind, discard, pure)
import Contract.Test.Browser (TestOptions, parseOptions)
import Contract.Test.Examples.Gero as Gero
import Contract.Test.Examples.Pkh2PkhGero as Pkh2PkhGero
import Contract.Test.Examples.Pkh2Pkh as Pkh2Pkh
import Contract.Test.Examples.AlwaysMints as AlwaysMints
import Contract.Test.Examples.AlwaysSucceeds as AlwaysSucceeds
import Contract.Test.Examples.Datums as Datums
import Contract.Test.Examples.MintsMultipleTokens as MintsMultipleTokens
import Contract.Test.Examples.SignMultiple as SignMultiple
import Test.Spec.Runner as SpecRunner
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Contract.Test.E2E`
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
  AlwaysSucceeds.runExample options
  Datums.runExample options
  MintsMultipleTokens.runExample options
  SignMultiple.runExample options
