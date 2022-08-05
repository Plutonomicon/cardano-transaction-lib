module Test.Base64 (suite) where

import Prelude

import Base64 (fromByteArray, mkBase64String, toByteArray, unBase64String)
import Data.Maybe (Maybe(Just))
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.QuickCheck (quickCheck, (===))
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  group "Base64" do
    test "toByteArray . fromByteArray = id" $ liftEffect do
      quickCheck \bytes ->
        toByteArray (fromByteArray bytes) === bytes
    test "mkBase64String <<< unBase64String = Just" $ liftEffect do
      quickCheck \base64Str ->
        mkBase64String (unBase64String base64Str) === Just base64Str
