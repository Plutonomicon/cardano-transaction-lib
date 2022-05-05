module Test.Metadata.Seabug
  ( suite
  ) where

import Prelude

import Data.Maybe (Maybe(Just))
import FromData (fromData)
import Metadata.MetadataType (fromGeneralTxMetadata, toGeneralTxMetadata)
import Mote (group, test)
import Test.Fixtures (seabugMetadataFixture1, seabugMetadataDeltaFixture1)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (toData)

suite :: TestPlanM Unit
suite = do
  group "Seabug Metadata" $ do
    test "MetadataType instance" do
      fromGeneralTxMetadata (toGeneralTxMetadata seabugMetadataFixture1)
        `shouldEqual` Just seabugMetadataFixture1
    test "FromData / ToData instances" do
      fromData (toData seabugMetadataFixture1) `shouldEqual` Just
        seabugMetadataFixture1
  group "Seabug Metadata delta" $ do
    test "FromData / ToData instances" do
      fromData (toData seabugMetadataDeltaFixture1) `shouldEqual` Just
        seabugMetadataDeltaFixture1
