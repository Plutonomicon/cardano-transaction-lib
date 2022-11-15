module Test.Ctl.Undefinable
  ( main
  , suite
  ) where

import Prelude hiding (join)

import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Ctl.Internal.Undefinable (fromUndefinable, toUndefinable)
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

-- Run with `spago test --main Test.Ctl.Undefinable`
main :: Effect Unit
main = launchAff_ do
  interpret suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Undefinable works correctly" do
    test "roundTrip Just" $ do
      (fromUndefinable $ toUndefinable $ Just 1) `shouldEqual` (Just 1)
    test "roundTrip Nothing" $ do
      let nothing = (Nothing :: Maybe Int)
      (fromUndefinable $ toUndefinable $ nothing) `shouldEqual` nothing
    test "roundTrip Nothing for record type" $ do
      let nothing = (Nothing :: Maybe { b :: Int })
      (fromUndefinable $ toUndefinable nothing) `shouldEqual` nothing
