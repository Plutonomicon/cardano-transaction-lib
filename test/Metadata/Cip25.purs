module Test.Metadata.Cip25 (suite) where

import Prelude

import Aeson (decodeAeson)
import Ctl.Internal.Test.Utils (TestPlanM)
import Data.Either (Either(Right), hush)
import Data.Maybe (Maybe(Just))
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoding (encodeUtf8)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FromData (fromData)
import Metadata.Cip25.Cip25String
  ( fromDataString
  , toCip25Strings
  , toDataString
  , fromMetadataString
  , toMetadataString
  )
import Metadata.MetadataType (fromGeneralTxMetadata, toGeneralTxMetadata)
import Mote (group, test)
import Test.Fixtures
  ( cip25MetadataFixture1
  , cip25MetadataFixture2
  , cip25MetadataFixture3
  , cip25MetadataJsonFixture1
  , cip25MetadataJsonFixture2
  , unsafeMkCip25String
  )
import Test.QuickCheck ((===))
import Test.QuickCheck.Combinators ((==>))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import ToData (toData)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "CIP25 Metadata" do
    test "Long string ToData encoding" do
      -- decodeUtf8 is not an inverse of encodeUtf8
      quickCheck $ \str -> do
        (hush (decodeUtf8 (encodeUtf8 str)) === Just str) ==>
          (fromDataString (toDataString str) === Just str)
    test "Long string ToMetadata encoding" do
      -- decodeUtf8 is not an inverse of encodeUtf8
      quickCheck $ \str -> do
        (hush (decodeUtf8 (encodeUtf8 str)) === Just str) ==>
          (fromMetadataString (toMetadataString str) === Just str)
    test "toCip25Strings #1" do
      toCip25Strings "asd" `shouldEqual` [ unsafeMkCip25String "asd" ]
    test "toCip25Strings #2" do
      toCip25Strings str80Chars
        `shouldEqual`
          [ unsafeMkCip25String
              "0123456789012345678901234567890123456789012345678901234567890123"
          , unsafeMkCip25String "4567890123456789"
          ]
    test "toCip25Strings #3" do
      toCip25Strings str160Chars
        `shouldEqual`
          [ unsafeMkCip25String
              "0123456789012345678901234567890123456789012345678901234567890123"
          , unsafeMkCip25String
              "4567890123456789012345678901234567890123456789012345678901234567"
          , unsafeMkCip25String "89012345678901234567890123456789"
          ]
    test "toCip25Strings #4" do
      toCip25Strings
        "0123456789012345678901234567890123456789012345678901234567890123"
        `shouldEqual`
          [ unsafeMkCip25String
              "0123456789012345678901234567890123456789012345678901234567890123"
          ]
    test "MetadataType instance #1" do
      fromGeneralTxMetadata (toGeneralTxMetadata cip25MetadataFixture1)
        `shouldEqual` Just cip25MetadataFixture1
    test "MetadataType instance #2" do
      fromGeneralTxMetadata (toGeneralTxMetadata cip25MetadataFixture2)
        `shouldEqual` Just cip25MetadataFixture2
    test "MetadataType instance #3" do
      fromGeneralTxMetadata (toGeneralTxMetadata cip25MetadataFixture3)
        `shouldEqual` Just cip25MetadataFixture3
    test "FromData / ToData instances #1" do
      fromData (toData cip25MetadataFixture1) `shouldEqual`
        Just cip25MetadataFixture1
    test "FromData / ToData instances #2" do
      fromData (toData cip25MetadataFixture2) `shouldEqual`
        Just cip25MetadataFixture2
    test "FromData / ToData instances #3" do
      fromData (toData cip25MetadataFixture3) `shouldEqual`
        Just cip25MetadataFixture3
    test "DecodeJson instance #1" do
      jsonFixture <- liftEffect cip25MetadataJsonFixture1
      decodeAeson jsonFixture `shouldEqual`
        Right cip25MetadataFixture1
    test "DecodeJson instance #2" do
      jsonFixture <- liftEffect cip25MetadataJsonFixture2
      decodeAeson jsonFixture `shouldEqual`
        Right cip25MetadataFixture2

str40Chars :: String
str40Chars = "0123456789012345678901234567890123456789"

str80Chars :: String
str80Chars = str40Chars <> str40Chars

str160Chars :: String
str160Chars = str80Chars <> str80Chars
