module Test.Examples.Datums (runExample) where

import Prelude

import Test.E2E.Browser (TestOptions, WalletExt(NamiExt))
import Test.E2E.Helpers
  ( namiSign
  , namiConfirmAccess
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Datums" options NamiExt
  $ \example -> do
      namiConfirmAccess example
      namiSign example
