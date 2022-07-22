module Contract.Test.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Examples (namiSign, namiConfirmAccess, runE2ETest)
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "MintsMultipleTokens" options NamiExt $
  \example -> do
    namiConfirmAccess example
    namiSign example
