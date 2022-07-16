module Contract.Test.Examples.SignMultiple (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Helpers
  ( namiSign
  , namiConfirmAccess
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "SignMultiple" options NamiExt $ \example -> do
  namiConfirmAccess example
  namiSign example
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delaySec 1.0
  namiSign example
