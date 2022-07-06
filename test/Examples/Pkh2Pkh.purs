module Test.Examples.Pkh2Pkh (testPkh2Pkh) where

import Prelude

import Effect.Console (log)
import Data.Newtype (wrap)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Mote (test)
import Test.E2E.Helpers
  ( startExample
  , checkSuccess
  , clickButton
  , password
  , reactSetValue
  , showOutput
  , testPassword
  , exampleUrl
  , namiConfirmAccess
  , namiSign
  )
import TestM (TestPlanM)
import Toppokki as Toki

testPkh2Pkh :: Toki.Browser -> TestPlanM Unit
testPkh2Pkh browser = test "Pkh2Pkh" do
  example <- startExample exampleUrl browser
  namiConfirmAccess example
  namiSign example
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delay (wrap 1000.0)
  checkSuccess example

