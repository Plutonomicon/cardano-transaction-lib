-- | This module contains definitions common for CIP-25 V1 and V2 standards
-- | (and probably future versions).
-- | https://cips.cardano.org/cips/cip25/
module Ctl.Internal.Metadata.Cip25.Common
  ( nftMetadataLabel
  , Cip25TokenName(Cip25TokenName)
  , Cip25MetadataFile(Cip25MetadataFile)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , (.:)
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Metadata.Cip25.Cip25String
  ( Cip25String
  , fromDataString
  , fromMetadataString
  , toDataString
  , toMetadataString
  )
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Ctl.Internal.Metadata.Helpers
  ( errExpectedObject
  , lookupKey
  , lookupMetadata
  )
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Plutus.Types.AssocMap as AssocMap
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.TokenName (TokenName, getTokenName, mkTokenName)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

nftMetadataLabel :: BigInt
nftMetadataLabel = BigInt.fromInt 721

-- | A newtype over `TokenName` that uses correct Json encoding (without `0x` prefix)
newtype Cip25TokenName = Cip25TokenName TokenName

derive newtype instance Eq Cip25TokenName
derive newtype instance Ord Cip25TokenName
derive newtype instance ToData Cip25TokenName
derive newtype instance FromData Cip25TokenName
derive newtype instance ToMetadata Cip25TokenName
derive newtype instance FromMetadata Cip25TokenName
derive instance Newtype Cip25TokenName _

instance Show Cip25TokenName where
  show (Cip25TokenName tn) = "(Cip25TokenName " <> show tn <> ")"

instance DecodeAeson Cip25TokenName where
  decodeAeson = (note (TypeMismatch "TokenName") <<< map wrap <<< mkTokenName)
    <=< decodeAeson

instance EncodeAeson Cip25TokenName where
  encodeAeson = encodeAeson <<< getTokenName <<< unwrap

-- | `files_details` in CDDL
-- |
-- | Same for V1 and V2.
-- |
-- | ```
-- | files_details =
-- |   {
-- |     name : string,
-- |     mediaType : string,
-- |     src : string / [* string]
-- |   }
-- | ```
newtype Cip25MetadataFile = Cip25MetadataFile
  { name :: Cip25String
  , mediaType :: Cip25String
  , src :: String
  }

derive instance Generic Cip25MetadataFile _
derive instance Newtype Cip25MetadataFile _
derive instance Eq Cip25MetadataFile

instance Show Cip25MetadataFile where
  show = genericShow

instance ToMetadata Cip25MetadataFile where
  toMetadata (Cip25MetadataFile file) = toMetadata
    [ "name" /\ toMetadata file.name
    , "mediaType" /\ toMetadata file.mediaType
    , "src" /\ toMetadataString file.src
    ]

instance FromMetadata Cip25MetadataFile where
  fromMetadata contents = do
    name <- lookupMetadata "name" contents >>= fromMetadata
    mediaType <- lookupMetadata "mediaType" contents >>= fromMetadata
    src <- lookupMetadata "src" contents >>= fromMetadataString
    pure $ wrap { name, mediaType, src }

instance ToData Cip25MetadataFile where
  toData (Cip25MetadataFile file) = toData $ AssocMap.Map $
    [ "name" /\ toData file.name
    , "mediaType" /\ toData file.mediaType
    , "src" /\ toDataString file.src
    ]

instance FromData Cip25MetadataFile where
  fromData contents = do
    name <- lookupKey "name" contents >>= fromData
    mediaType <- lookupKey "mediaType" contents >>= fromData
    src <- lookupKey "src" contents >>= fromDataString
    pure $ wrap { name, mediaType, src }

instance DecodeAeson Cip25MetadataFile where
  decodeAeson =
    caseAesonObject errExpectedObject \obj -> do
      name <- obj .: "name"
      mediaType <- obj .: "mediaType"
      src <- obj .: "src"
      pure $ wrap { name, mediaType, src }

instance EncodeAeson Cip25MetadataFile where
  encodeAeson (Cip25MetadataFile { name, mediaType, src }) = encodeAeson
    { name
    , mediaType
    , src
    }
