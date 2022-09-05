module Test.E2E.Examples.Datums (runExample) where

import Prelude

import Contract.Test.E2E
  ( TestOptions
  , SomeWallet(SomeWallet)
  , WalletExt(NamiExt)
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.E2E.Helpers
  ( runE2ETest
  )
import TestM (TestPlanM)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { id, confirmAccess, sign }) password options =
  runE2ETest "Datums" options NamiExt
    $ \example -> do
        confirmAccess id example
        sign id password example
