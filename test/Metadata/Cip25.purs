module Test.Metadata.Cip25 (suite) where

import Prelude

import Aeson (decodeAeson)
import Effect.Class (liftEffect)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just))
import Metadata.MetadataType (fromGeneralTxMetadata, toGeneralTxMetadata)
import Mote (group, test)
import FromData (fromData)
import ToData (toData)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Test.Fixtures (cip25MetadataFixture1, cip25MetadataJsonFixture1)

suite :: TestPlanM Unit
suite = do
  group "CIP25 Metadata" do
    test "MetadataType instance" do
      fromGeneralTxMetadata (toGeneralTxMetadata cip25MetadataFixture1)
        `shouldEqual` Just cip25MetadataFixture1
    test "FromData / ToData instances" do
      fromData (toData cip25MetadataFixture1) `shouldEqual`
        Just cip25MetadataFixture1
    test "DecodeJson instance" do
      jsonFixture <- liftEffect cip25MetadataJsonFixture1
      decodeAeson jsonFixture `shouldEqual`
        Right cip25MetadataFixture1
