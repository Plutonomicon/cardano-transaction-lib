module Test.CTL.E2E.Examples.AlwaysMints (runExample) where

import Prelude

import CTL.Contract.Test.E2E
  ( SomeWallet(SomeWallet)
  , TestOptions
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.CTL.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import Test.CTL.TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, wallet, confirmAccess, sign }) password options =
  runE2ETest "AlwaysMints" options wallet $ \example -> do
    confirmAccess id example
    delaySec 3.0
    sign id password example
