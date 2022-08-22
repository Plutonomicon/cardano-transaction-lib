module Test.E2E.Examples.AlwaysMints (runExample) where

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

runExample :: TestOptions -> TestPlanM (Aff Unit) Unit
runExample options = runE2ETest "AlwaysMints" options NamiExt $ \example -> do
  namiConfirmAccess example
  delaySec 3.0
  namiSign' example
