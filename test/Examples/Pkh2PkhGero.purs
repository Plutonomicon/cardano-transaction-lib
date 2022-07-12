module Test.Examples.Pkh2PkhGero (runExample) where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (delay)
import Mote (test)
import Effect.Class (liftEffect)
import Effect.Console (log)
import TestM (TestPlanM)
import Test.E2E.Browser (TestOptions, launchWithExtension)
import Test.E2E.Helpers
  ( checkSuccess
  , geroSign
  , geroConfirmAccess
  , startExample
  , delaySec
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Pkh2PkhGero" options "Gero" $ \example -> do
  liftEffect $ log "1!"
  geroConfirmAccess example
  liftEffect $ log "2!"  
  geroSign example  
  liftEffect $ log "3!"


