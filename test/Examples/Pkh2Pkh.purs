module Test.Examples.Pkh2Pkh where

import Prelude

import Data.Maybe (isJust, Maybe(..))
import Data.Newtype (wrap)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (test)
import Test.E2E.Helpers
  ( NoShowPage(NoShowPage)
  , clickButton
  , findNamiPage
  , injectJQueryAll
  , password
  , reactSetValue
  , retrieveJQuery
  , testPassword
  )
import Test.E2E.Wallet (Mode(..), launchWithNami)
import Test.E2E.Feedback (testFeedbackIsTrue)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Toppoki (example)
import TestM (TestPlanM)
import Toppokki as Toki

testPkh2Pkh :: TestPlanM Unit
testPkh2Pkh = test "Pkh2Pkh" do
  browser <- launchWithNami Headless
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  Toki.goto (wrap example) page
  -- find a nicer way for waiting
  delay (wrap 5000.0)
  injectJQueryAll jQuery browser
  namiPage <- findNamiPage browser
  shouldSatisfy (NoShowPage <$> namiPage) isJust
  case namiPage of
    -- I'm really unsure how to deal with this re. code guidelines...
    Nothing -> liftEffect $ throw "Impossible"
    Just np -> do
      clickButton "Access" np -- this matches when the wallet is used the first time
      clickButton "Sign" np
      reactSetValue password testPassword np
      clickButton "Confirm" np
  feedback <- testFeedbackIsTrue page
  shouldSatisfy feedback (_ == true)

