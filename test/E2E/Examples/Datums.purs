module Test.CTL.E2E.Examples.Datums (runExample) where

import Prelude

import CTL.Contract.Test.E2E
  ( SomeWallet(SomeWallet)
  , TestOptions
  , WalletExt(NamiExt)
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.CTL.E2E.Helpers
  ( runE2ETest
  )
import Test.CTL.TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, confirmAccess, sign }) password options =
  runE2ETest "Datums" options NamiExt
    $ \example -> do
        confirmAccess id example
        sign id password example
