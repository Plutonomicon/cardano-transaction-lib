module Contract.Test.Examples.Datums (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Helpers
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
