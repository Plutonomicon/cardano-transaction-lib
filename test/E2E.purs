module Test.CTL.E2E (main) where

import CTL.Contract.Test.E2E (TestOptions(TestOptions), parseOptions)
import CTL.Contract.Test.E2E.WalletExt
  ( WalletConfig(WalletConfig)
  , getWalletByType
  )
import Data.Functor (void)
import Data.Newtype (wrap)
import Data.TraversableWithIndex (forWithIndex)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Mote (group)
import Prelude (Unit, bind, discard, pure, ($))
import Test.CTL.E2E.Examples.AlwaysMints as AlwaysMints
import Test.CTL.E2E.Examples.AlwaysSucceeds as AlwaysSucceeds
import Test.CTL.E2E.Examples.Datums as Datums
import Test.CTL.E2E.Examples.MintsMultipleTokens as MintsMultipleTokens
import Test.CTL.E2E.Examples.Pkh2Pkh as Pkh2Pkh
import Test.CTL.E2E.Examples.SignMultiple as SignMultiple
import Test.CTL.TestM (TestPlanM)
import Test.CTL.Utils as Utils
import Test.Spec.Runner as SpecRunner

-- Run with `spago test --main Test.CTL.E2E`
main :: Effect Unit
main = launchAff_ $ do
  options <- liftEffect parseOptions
  Utils.interpretWithConfig
    (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    (testPlan options)

-- Requires external services listed in README.md
testPlan :: TestOptions -> TestPlanM (Aff Unit) Unit
testPlan options@(TestOptions { wallets }) = group "e2e tests"
  $ void
  $ forWithIndex wallets
  $ \wallet (WalletConfig _ password) -> do
      w <- liftEffect $ getWalletByType wallet
      Pkh2Pkh.runExample w password options
      AlwaysMints.runExample w password options
      AlwaysSucceeds.runExample w password options
      Datums.runExample w password options
      MintsMultipleTokens.runExample w password options
      SignMultiple.runExample w password options
