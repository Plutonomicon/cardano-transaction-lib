module Test.Metadata.Seabug
  ( suite
  ) where

import Prelude

import Data.Maybe (Maybe(Just))
import FromData (fromData)
import Mote (group, test)
import Test.Fixtures (seabugMetadataFixture1)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (toData)

suite :: TestPlanM Unit
suite = do
  group "Seabug Metadata" $ do
    test "Fixture 1" do
      fromData (toData seabugMetadataFixture1) `shouldEqual` Just seabugMetadataFixture1
