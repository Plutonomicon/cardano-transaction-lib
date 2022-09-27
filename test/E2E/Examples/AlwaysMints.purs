module Test.Ctl.E2E.Examples.AlwaysMints (runExample) where

import Prelude

import Contract.Test.E2E
  ( SomeWallet(SomeWallet)
  , TestOptions
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.Ctl.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import Test.Ctl.TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, wallet, confirmAccess, sign }) password options =
  runE2ETest "AlwaysMints" options wallet $ \example -> do
    confirmAccess id example
    delaySec 3.0
    sign id password example
