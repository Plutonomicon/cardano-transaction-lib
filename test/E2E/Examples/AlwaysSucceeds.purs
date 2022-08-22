module Test.E2E.Examples.AlwaysSucceeds (runExample) where

import Prelude

import Contract.Test.E2E (TestOptions, WalletExt(NamiExt))
import Effect.Aff (Aff)
import Test.E2E.Helpers
  ( namiSign'
  , namiConfirmAccess
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)
import Effect.Console (log)
import Effect.Class (liftEffect)

runExample :: TestOptions -> TestPlanM (Aff Unit) Unit
runExample options = runE2ETest "AlwaysSucceeds" options NamiExt $ \example ->
  do
    namiConfirmAccess example
    namiSign' example
    liftEffect $ log $
      " ...waiting before trying to spend script output (this will take a minute)"
    delaySec 65.0
    namiSign' example
