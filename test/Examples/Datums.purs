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
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delaySec 1.0
  checkSuccess example
