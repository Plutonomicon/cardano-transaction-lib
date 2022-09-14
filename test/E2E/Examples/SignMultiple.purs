module Test.CTL.E2E.Examples.SignMultiple (runExample) where

import Prelude

import CTL.Contract.Test.E2E (SomeWallet(SomeWallet), TestOptions, WalletPassword)
import Effect.Aff (Aff)
import Test.CTL.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import Test.CTL.TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, wallet, confirmAccess, sign }) password options =
  runE2ETest "SignMultiple" options wallet \example -> do
    confirmAccess id example
    sign id password example
    -- Wait a moment to avoid a race condition. After Nami gets confirmation,
    -- it will take a few ms to return control to our example.
    delaySec 1.0
    sign id password example
