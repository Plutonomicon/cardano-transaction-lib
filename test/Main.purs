module Test.Main (main) where

import Prelude

import Ctl.Test.Unit as Unit
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Integration as Integration
import Test.Utils as Utils

main :: Effect Unit
main = launchAff_ do
  Utils.interpret do
    Unit.testPlan
    Integration.testPlan
