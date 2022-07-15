module Test.Examples.AlwaysSucceeds (runExample) where

import Prelude

import Test.E2E.Browser (TestOptions, WalletExt(NamiExt))
import Test.E2E.Helpers
  ( namiSign
  , namiConfirmAccess
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "AlwaysSucceeds" options NamiExt $ \example -> do
  namiConfirmAccess example
  namiSign example
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delaySec 65.0

