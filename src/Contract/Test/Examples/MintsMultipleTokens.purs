module Contract.Test.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Contract.Test.Browser (TestOptions, WalletExt(NamiExt))
import Contract.Test.Helpers (namiSign, namiConfirmAccess, runE2ETest)
import TestM (TestPlanM)
import Effect.Class (liftEffect)
import Effect.Console (log)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "MintsMultipleTokens" options NamiExt $
  \example -> do
    liftEffect $ log "Confirm Nami Access"
    namiConfirmAccess example
    liftEffect $ log "Sign Nami Transaction"
    namiSign example
