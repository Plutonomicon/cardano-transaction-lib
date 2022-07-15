module Test.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Test.E2E.Browser (TestOptions, WalletExt(NamiExt))
import Test.E2E.Helpers (namiSign, namiConfirmAccess, runE2ETest)
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
