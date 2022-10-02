module Test.Ctl.Main (main) where

import Prelude

import Ctl.Internal.Test.TestPlanM (interpret)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Ctl.Integration as Integration
import Test.Ctl.Unit as Unit

main :: Effect Unit
main = launchAff_ do
  interpret do
    Unit.testPlan
    Integration.testPlan
