module Test.Ctl.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Ctl.Integration as Integration
import Test.Ctl.Unit as Unit
import Test.Ctl.Utils as Utils

main :: Effect Unit
main = launchAff_ do
  Utils.interpret do
    Unit.testPlan
    Integration.testPlan
