module Metadata.CIP25
  ( CIP25Metadata(CIP25Metadata)
  , CIP25MetadataEntry(CIP25MetadataEntry)
  , CIP25MetadataFile(CIP25MetadataFile)
  , CIP25MetadataEntryRecord
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Data.Array (concat, groupBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head) as NonEmpty
import Data.NonEmpty (NonEmpty)
import Data.Map (fromFoldable, singleton) as Map
import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Traversable (for, sequence)
import Partial.Unsafe (unsafePartial)
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Types.Scripts (MintingPolicyHash)
import Types.Value (TokenName)
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Map))
import Metadata.Helpers(mkKey, lookupKey)

nftMetadataLabel :: String
nftMetadataLabel = "721"

--------------------------------------------------------------------------------
-- CIP25MetadataFile
--------------------------------------------------------------------------------

newtype CIP25MetadataFile = CIP25MetadataFile
  { name :: ByteArray
  , mediaType :: ByteArray
  , uris :: NonEmpty Array ByteArray
  }

derive instance Generic CIP25MetadataFile _
derive instance Newtype CIP25MetadataFile _
derive instance Eq CIP25MetadataFile

instance Show CIP25MetadataFile where
  show = genericShow

instance ToData CIP25MetadataFile where
  toData (CIP25MetadataFile meta) = unsafePartial $ toData $ Map.fromFoldable $
    [ mkKey "name" /\ toData meta.name
    , mkKey "mediaType" /\ toData meta.mediaType
    , mkKey "src" /\ toData meta.uris
    ]

instance FromData CIP25MetadataFile where
  fromData contents = unsafePartial do
    name <- lookupKey "name" contents >>= fromData
    mediaType <- lookupKey "mediaType" contents >>= fromData
    uris <- lookupKey "uris" contents >>= fromData
    pure $ CIP25MetadataFile { name, mediaType, uris }

--------------------------------------------------------------------------------
-- CIP25MetadataEntry
--------------------------------------------------------------------------------

type CIP25MetadataEntryRecord =
  { policyId :: MintingPolicyHash
  , assetName :: TokenName
  , imageUris :: NonEmpty Array ByteArray
  , mediaType :: Maybe ByteArray
  , description :: Array ByteArray
  , files :: Array CIP25MetadataFile
  }

newtype CIP25MetadataEntry = CIP25MetadataEntry CIP25MetadataEntryRecord

derive instance Generic CIP25MetadataEntry _
derive instance Newtype CIP25MetadataEntry _
derive instance Eq CIP25MetadataEntry

instance Show CIP25MetadataEntry where
  show = genericShow

metadataEntryToData :: CIP25MetadataEntry -> PlutusData
metadataEntryToData (CIP25MetadataEntry meta) = unsafePartial $ toData $
  Map.fromFoldable
    [ mkKey "name" /\ toData meta.assetName
    , mkKey "image" /\ toData meta.imageUris
    , mkKey "mediaType" /\ toData meta.mediaType
    , mkKey "description" /\ toData meta.description
    , mkKey "files" /\ toData meta.files
    ]

metadataEntryFromData
  :: MintingPolicyHash
  -> TokenName
  -> PlutusData
  -> Maybe CIP25MetadataEntry
metadataEntryFromData policyId assetName contents = unsafePartial do
  imageUris <- lookupKey "image" contents >>= fromData
  mediaType <- lookupKey "mediaType" contents >>= fromData
  description <- lookupKey "description" contents >>= fromData
  files <- lookupKey "files" contents >>= fromData
  pure $ CIP25MetadataEntry
    { policyId
    , assetName
    , imageUris
    , mediaType
    , description
    , files
    }

--------------------------------------------------------------------------------
-- CIP25Metadata
--------------------------------------------------------------------------------

newtype CIP25Metadata = CIP25Metadata (Array CIP25MetadataEntry)

derive instance Generic CIP25Metadata _
derive instance Newtype CIP25Metadata _
derive instance Eq CIP25Metadata

instance Show CIP25Metadata where
  show = genericShow

instance ToData CIP25Metadata where
  toData (CIP25Metadata entries) = unsafePartial $ toData
    $ Map.singleton (mkKey nftMetadataLabel)
    $ Map.fromFoldable
    $ flip map groups
    $ \group ->
        toData (_.policyId $ NonEmpty.head group) /\
          (Map.fromFoldable <<< flip map group) \entry ->
            toData (entry.assetName) /\ metadataEntryToData (wrap entry)
    where
    groups :: Array (NonEmptyArray CIP25MetadataEntryRecord)
    groups = flip groupBy (unwrap <$> entries) $
      \a b -> a.policyId == b.policyId

instance FromData CIP25Metadata where
  fromData meta = unsafePartial $ do
    entries <- lookupKey nftMetadataLabel meta >>= case _ of
      Map mp1 -> map concat
        $ for mp1
        $ \(policyId /\ assets) ->
            case assets of
              Map mp2 ->
                for mp2 $ \(assetName /\ contents) ->
                  metadataEntryFromData <$> fromData policyId
                    <*> fromData assetName
                    <*> pure contents
              _ -> Nothing
      _ -> Nothing
    wrap <$> sequence entries
