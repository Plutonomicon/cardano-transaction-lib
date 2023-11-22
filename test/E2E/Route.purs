module Test.Ctl.E2E.Route where

import Prelude

import Ctl.Internal.Test.E2E.Route (parseRoute)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Either (isRight)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "E2E router" do
    group "parseRoute" do
      test "#1" do
        parseRoute "E2EConfigName:E2ETestName" `shouldSatisfy` isRight
      test "#2" do
        parseRoute
          "E2EConfigName:E2ETestName:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209"
          `shouldSatisfy` isRight
      test "#3" do
        parseRoute
          "E2EConfigName:E2ETestName:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209:5820f0db841df6c7fbc4506c58fad6676db0354a02dfd26efca445715a8adeabc338"
          `shouldSatisfy` isRight
