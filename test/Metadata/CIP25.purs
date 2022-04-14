module Test.Metadata.CIP25 (suite) where

import Prelude

import Data.Argonaut (decodeJson)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just))
import Mote (group, test)
import FromData (fromData)
import ToData (toData)
import Test.Fixtures (cip25MetadataFixture1)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Test.Fixtures (cip25MetadataFixture1, cip25MetadataJsonFixture1)

suite :: TestPlanM Unit
suite = do
  group "CIP25 Metadata" do
    test "FromData / ToData instances" do
      fromData (toData cip25MetadataFixture1) `shouldEqual`
        Just cip25MetadataFixture1
    test "DecodeJson instance" do
      decodeJson cip25MetadataJsonFixture1 `shouldEqual`
        Right cip25MetadataFixture1
