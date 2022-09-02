module Test.E2E.Examples.AlwaysSucceeds (runExample) where

import Prelude

import Contract.Test.E2E
  ( TestOptions
  , SomeWallet(SomeWallet)
  , WalletExt(NamiExt)
  , WalletPassword
  )
import Effect.Aff (Aff)
import Test.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import TestM (TestPlanM)
import Effect.Console (log)
import Effect.Class (liftEffect)

runExample
  :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet { wallet, confirmAccess, sign }) password options =
  runE2ETest "AlwaysSucceeds" options wallet $ \example ->
    do
      confirmAccess example
      sign password example
      liftEffect $ log $
        " ...waiting before trying to spend script output (this will take a minute)"
      delaySec 65.0
      sign password example

