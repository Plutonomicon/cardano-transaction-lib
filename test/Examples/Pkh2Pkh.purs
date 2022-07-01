module Test.Examples.Pkh2Pkh where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (delay)
import Mote (test)
import Test.E2E.Feedback (testFeedbackIsTrue)
import Test.E2E.Helpers
  ( ExamplePages(ExamplePages)
  , startExample
  , clickButton
  , password
  , reactSetValue
  , testPassword
  , exampleUrl
  )
import Test.Spec.Assertions (shouldSatisfy)
import TestM (TestPlanM)
import Toppokki as Toki

testPkh2Pkh :: Toki.Browser -> TestPlanM Unit
testPkh2Pkh browser = test "Pkh2Pkh" do
  ExamplePages { nami, main } <- startExample exampleUrl browser
  clickButton "Access" nami -- this matches when the wallet is used the first time
  clickButton "Sign" nami
  reactSetValue password testPassword nami
  clickButton "Confirm" nami
  -- Wait a moment to avoid a race condition. After Nami gets confirmation,
  -- it will take a few ms to return control to our example.
  delay (wrap 1000.0)
  feedback <- testFeedbackIsTrue main
  shouldSatisfy feedback (_ == true)

