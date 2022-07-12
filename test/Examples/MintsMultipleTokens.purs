module Test.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (delay)
import Mote (test)
import Test.E2E.Browser (TestOptions, launchWithExtension)
import Test.E2E.Helpers
  ( checkSuccess
  , namiSign
  , namiConfirmAccess
  , startExample
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)
import Toppokki as Toppokki
import Effect.Class (liftEffect)
import Effect.Console (log)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "MintsMultipleTokens" options "Nami" $
  \example -> do
    liftEffect $ log "Confirm Nami Access"
    namiConfirmAccess example
    liftEffect $ log "Sign Nami Transaction"
    namiSign example
