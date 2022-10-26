module Test.Ctl.Base64 (suite) where

import Prelude

import Ctl.Internal.Base64
  ( fromByteArray
  , mkBase64String
  , toByteArray
  , unBase64String
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.QuickCheck (quickCheck, (===))

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Base64" do
    test "toByteArray . fromByteArray = id" $ liftEffect do
      quickCheck \bytes ->
        toByteArray (fromByteArray bytes) === bytes
    test "mkBase64String <<< unBase64String = Just" $ liftEffect do
      quickCheck \base64Str ->
        mkBase64String (unBase64String base64Str) === Just base64Str
