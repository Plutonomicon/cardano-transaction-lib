module Test.Examples.Pkh2Pkh (testPkh2Pkh) where

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
  )
import TestM (TestPlanM)
import Toppokki as Toppokki

testPkh2Pkh :: TestOptions -> TestPlanM Unit
testPkh2Pkh options = test "Pkh2Pkh" do
  delaySec 1.0
  browser <- launchWithExtension options "Nami"
  example <- startExample "Pkh2Pkh" browser
  namiConfirmAccess example
  namiSign example
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delaySec 1.0
  checkSuccess example
  Toppokki.close browser

