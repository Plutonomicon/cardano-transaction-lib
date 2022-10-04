module Test.Ctl.E2E.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Contract.Test.E2E
  ( SomeWallet(SomeWallet)
  , TestOptions
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.Ctl.E2E.Helpers (runE2ETest)
import Test.Ctl.TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, wallet, confirmAccess, sign }) password options =
  runE2ETest "MintsMultipleTokens" options wallet $
    \example -> do
      confirmAccess id example
      sign id password example
