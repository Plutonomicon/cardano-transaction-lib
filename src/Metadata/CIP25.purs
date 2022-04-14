module Metadata.CIP25
  ( CIP25Metadata(CIP25Metadata)
  , CIP25MetadataEntry(CIP25MetadataEntry)
  , CIP25MetadataFile(CIP25MetadataFile)
  , CIP25MetadataEntryRecord
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut (class DecodeJson, Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Array (uncons, concat, groupBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head) as NonEmpty
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable, singleton) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse, sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object, toUnfoldable) as FO
import Partial.Unsafe (unsafePartial)
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Serialization.Hash (scriptHashFromBytes)
import Types.Scripts (MintingPolicyHash)
import Types.Value (TokenName, mkTokenName)
import Types.ByteArray (ByteArray, byteArrayFromString, hexToByteArray)
import Types.PlutusData (PlutusData(Map))
import Metadata.Helpers (unsafeMkKey, lookupKey)

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
    [ unsafeMkKey "name" /\ toData meta.name
    , unsafeMkKey "mediaType" /\ toData meta.mediaType
    , unsafeMkKey "src" /\ toData meta.uris
    ]

instance FromData CIP25MetadataFile where
  fromData contents = unsafePartial do
    name <- lookupKey "name" contents >>= fromData
    mediaType <- lookupKey "mediaType" contents >>= fromData
    uris <- lookupKey "src" contents >>= fromData
    pure $ wrap { name, mediaType, uris }

instance DecodeJson CIP25MetadataFile where
  decodeJson =
    Json.caseJsonObject errExpectedObject $ \obj -> do
      name <- obj .: "name" >>=
        note errInvalidByteArray
          <<< decodeByteArray
      mediaType <- obj .: "mediaType" >>=
        note errInvalidByteArray
          <<< decodeByteArray
      uris <- decodeNonEmptyStringArray =<< obj .: "src"
      pure $ wrap { name, mediaType, uris }
    where
    errInvalidByteArray =
      Json.TypeMismatch "Invalid ByteArray"

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

metadataEntryToData_ :: CIP25MetadataEntry -> PlutusData
metadataEntryToData_ (CIP25MetadataEntry meta) = unsafePartial $ toData $
  Map.fromFoldable
    [ unsafeMkKey "name" /\ toData meta.assetName
    , unsafeMkKey "image" /\ toData meta.imageUris
    , unsafeMkKey "mediaType" /\ toData meta.mediaType
    , unsafeMkKey "description" /\ toData meta.description
    , unsafeMkKey "files" /\ toData meta.files
    ]

metadataEntryFromData_
  :: MintingPolicyHash
  -> TokenName
  -> PlutusData
  -> Maybe CIP25MetadataEntry
metadataEntryFromData_ policyId assetName contents = unsafePartial do
  imageUris <- lookupKey "image" contents >>= fromData
  mediaType <- lookupKey "mediaType" contents >>= fromData
  description <- lookupKey "description" contents >>= fromData
  files <- lookupKey "files" contents >>= fromData
  pure $
    wrap { policyId, assetName, imageUris, mediaType, description, files }

metadataEntryDecodeJson_
  :: MintingPolicyHash
  -> TokenName
  -> Json
  -> Either Json.JsonDecodeError CIP25MetadataEntry
metadataEntryDecodeJson_ policyId assetName =
  Json.caseJsonObject errExpectedObject $ \obj -> do
    imageUris <- obj .: "image" >>=
      decodeNonEmptyStringArray
    mediaType <- obj .:? "mediaType" >>=
      pure <<< bindFlipped decodeByteArray
    description <- obj .:? "description" >>=
      maybe (pure mempty) decodeStringArray
    files <- obj .:? "files" >>= \arr ->
      maybe (pure mempty) (traverse Json.decodeJson) (Json.toArray =<< arr)
    pure $
      wrap { policyId, assetName, imageUris, mediaType, description, files }

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
    $ Map.singleton (unsafeMkKey nftMetadataLabel)
    $ Map.fromFoldable
    $ flip map groups
    $ \group ->
        toData (_.policyId $ NonEmpty.head group) /\
          (Map.fromFoldable <<< flip map group) \entry ->
            toData (entry.assetName) /\ metadataEntryToData_ (wrap entry)
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
                  metadataEntryFromData_ <$> fromData policyId
                    <*> fromData assetName
                    <*> pure contents
              _ -> Nothing
      _ -> Nothing
    wrap <$> sequence entries

instance DecodeJson CIP25Metadata where
  decodeJson =
    Json.caseJsonObject errExpectedObject $ \obj -> do
      policies <- obj .: nftMetadataLabel
      caseJsonObject policies $ \objPolicies ->
        map (wrap <<< concat)
          $ for (objToArray objPolicies)
          $ \(policyId /\ assets) ->
              caseJsonObject assets $ \objAssets ->
                for (objToArray objAssets) $ \(assetName /\ contents) -> do
                  policyId_ <- decodePolicyId policyId
                  assetName_ <- decodeAssetName assetName
                  metadataEntryDecodeJson_ policyId_ assetName_ contents
    where
    objToArray :: forall a. FO.Object a -> Array (Tuple String a)
    objToArray = FO.toUnfoldable

    caseJsonObject
      :: forall a
       . Json
      -> (FO.Object Json -> Either Json.JsonDecodeError a)
      -> Either Json.JsonDecodeError a
    caseJsonObject = flip (Json.caseJsonObject errExpectedObject)

    decodePolicyId :: String -> Either Json.JsonDecodeError MintingPolicyHash
    decodePolicyId =
      note (Json.TypeMismatch "Expected hex-encoded policy id")
        <<< map wrap
        <<< (scriptHashFromBytes <=< hexToByteArray)

    decodeAssetName :: String -> Either Json.JsonDecodeError TokenName
    decodeAssetName =
      note (Json.TypeMismatch "Expected UTF-8 encoded asset name")
        <<< (mkTokenName <=< byteArrayFromString)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

errExpectedObject :: forall a. Either Json.JsonDecodeError a
errExpectedObject =
  Left (Json.TypeMismatch "Expected object")

errExpectedArray :: forall a. Either Json.JsonDecodeError a
errExpectedArray =
  Left (Json.TypeMismatch "Expected array")

errExpectedNonEmptyArray :: forall a. Either Json.JsonDecodeError a
errExpectedNonEmptyArray =
  Left (Json.TypeMismatch "Expected non-empty array")

decodeByteArray :: Json -> Maybe ByteArray
decodeByteArray = byteArrayFromString <=< Json.toString

decodeStringArray
  :: Json -> Either Json.JsonDecodeError (Array ByteArray)
decodeStringArray json
  | Json.isString json =
      decodeStringArray (Json.jsonSingletonArray json)
  | otherwise =
      flip (Json.caseJsonArray errExpectedArray) json $ \arr ->
        pure $ fromMaybe mempty (traverse decodeByteArray arr)

decodeNonEmptyStringArray
  :: Json -> Either Json.JsonDecodeError (NonEmpty Array ByteArray)
decodeNonEmptyStringArray json =
  decodeStringArray json >>= \arr ->
    case uncons arr of
      Just { head, tail } -> pure (head :| tail)
      Nothing -> errExpectedNonEmptyArray
