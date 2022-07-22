module Contract.Test.Examples.AlwaysMints (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Examples
  ( namiSign
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
