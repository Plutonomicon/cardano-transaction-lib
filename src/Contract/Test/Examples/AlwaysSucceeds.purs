module Contract.Test.Examples.AlwaysSucceeds (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Helpers
  ( namiSign
  , namiConfirmAccess
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)
import Effect.Console (log)
import Effect.Class (liftEffect)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "AlwaysSucceeds" options NamiExt $ \example -> do
    namiConfirmAccess example
    namiSign example
    liftEffect $ log $ " ...waiting before trying to spend script output (this will take a minute)"
    delaySec 65.0

