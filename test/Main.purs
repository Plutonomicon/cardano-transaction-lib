module Test.CTL.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CTL.Integration as Integration
import Test.CTL.Unit as Unit
import Test.CTL.Utils as Utils

main :: Effect Unit
main = launchAff_ do
  Utils.interpret do
    Unit.testPlan
    Integration.testPlan
