module Test.Examples.Datums (runExample) where

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

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Datums" options "Nami" $ \example -> do
  namiConfirmAccess example
  namiSign example
