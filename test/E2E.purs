module Test.E2E (main) where

import Contract.Test.E2E (TestOptions, parseOptions)
import Contract.Test.E2E.Options (parseCommand, parseCommand)
import Contract.Test.E2E.Runner (runE2E)
import Contract.Test.E2E.WalletExt (getWalletByType)
import Data.Functor (void)
import Data.Newtype (wrap)
import Data.TraversableWithIndex (forWithIndex)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group)
import Prelude (Unit, bind, discard, pure, show, ($))
import Test.Spec.Runner as SpecRunner
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = launchAff_ $ do
  options <- liftEffect parseCommand
  -- Utils.interpretWithConfig
  --   (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
  --   (testPlan options)
  runE2E options
  liftEffect $ Console.log $ show options

-- -- Requires external services listed in README.md
-- testPlan :: TestOptions -> TestPlanM (Aff Unit) Unit
-- testPlan options@{ wallets } = group "e2e tests"
--   $ void
--   $ forWithIndex wallets
--   $ \wallet (WalletConfig _ password) -> do
--       w <- liftEffect $ getWalletByType wallet
--       Pkh2Pkh.runExample w password options
--       AlwaysMints.runExample w password options
--       AlwaysSucceeds.runExample w password options
--       Datums.runExample w password options
--       MintsMultipleTokens.runExample w password options
--       SignMultiple.runExample w password options
