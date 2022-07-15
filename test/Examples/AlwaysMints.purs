module Test.Examples.AlwaysMints (runExample) where

import Prelude

import Test.E2E.Browser (TestOptions, WalletExt(NamiExt))
import Test.E2E.Helpers
  ( checkSuccess
  , namiSign
  , namiConfirmAccess
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "AlwaysMints" options NamiExt $ \example -> do
  namiConfirmAccess example
  delaySec 3.0
  namiSign example
