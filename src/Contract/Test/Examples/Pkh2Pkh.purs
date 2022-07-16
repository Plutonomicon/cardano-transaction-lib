module Contract.Test.Examples.Pkh2Pkh (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Helpers
  ( namiSign
  , namiConfirmAccess
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Pkh2Pkh" options NamiExt $ \example -> do
  namiConfirmAccess example
  namiSign example
