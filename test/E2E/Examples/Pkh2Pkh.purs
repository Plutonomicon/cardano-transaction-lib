module Test.E2E.Examples.Pkh2Pkh (runExample) where

import Prelude

import Contract.Test.E2E (TestOptions, SomeWallet(SomeWallet), WalletPassword)
import Effect.Aff (Aff)
import Test.E2E.Helpers
  ( namiConfirmAccess
  , runE2ETest
  )
import TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { wallet, confirmAccess, sign }) password options =
  runE2ETest "Pkh2Pkh" options wallet $ \example -> do
    confirmAccess example
    sign password example
