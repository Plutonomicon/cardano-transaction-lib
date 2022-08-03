module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Integration as Integration
import Ctl.Test.Unit as Unit
import Test.Utils as Utils

main :: Effect Unit
main = launchAff_ do
  Utils.interpret do
    Unit.testPlan
    Integration.testPlan
