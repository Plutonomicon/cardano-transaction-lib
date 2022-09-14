module Test.CTL.E2E.Examples.AlwaysSucceeds (runExample) where

import Prelude

import CTL.Contract.Test.E2E (SomeWallet(SomeWallet), TestOptions, WalletPassword)
import Effect.Aff (Aff)
import Test.CTL.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import Test.CTL.TestM (TestPlanM)
import Effect.Console (log)
import Effect.Class (liftEffect)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, wallet, confirmAccess, sign }) password options =
  runE2ETest "AlwaysSucceeds" options wallet $ \example ->
    do
      confirmAccess id example
      sign id password example
      liftEffect $ log $
        " ...waiting before trying to spend script output (this will take a minute)"
      delaySec 65.0
      sign id password example
