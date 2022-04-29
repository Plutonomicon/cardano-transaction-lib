module Metadata.Cip25
  ( Cip25Metadata(Cip25Metadata)
  , Cip25MetadataEntry(Cip25MetadataEntry)
  , Cip25MetadataFile(Cip25MetadataFile)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Array (uncons, concat, groupBy)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Array.NonEmpty (head) as NonEmpty
import Data.BigInt (fromInt)
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Show.Generic (genericShow)
import Data.TextEncoder (encodeUtf8)
import Data.Traversable (for, traverse, sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object, toUnfoldable) as FO
import Plutus.Types.AssocMap (Map(Map), singleton) as AssocMap
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Metadata.Helpers (lookupKey)
import Metadata.ToMetadata
  ( class ToMetadata
  , class ToGeneralTransactionMetadata
  , toMetadata
  , anyToMetadata
  )
import Serialization.Hash (scriptHashFromBytes)
import Types.Scripts (MintingPolicyHash)
import Types.ByteArray (hexToByteArray)
import Types.PlutusData (PlutusData(Map))
import Types.TokenName (TokenName, mkTokenName)
import Types.TransactionMetadata (TransactionMetadatum)

nftMetadataLabel :: String
nftMetadataLabel = "721"

--------------------------------------------------------------------------------
-- Cip25MetadataFile
--------------------------------------------------------------------------------

newtype Cip25MetadataFile = Cip25MetadataFile
  { name :: String
  , mediaType :: String
  , uris :: NonEmpty Array String
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
    , "src" /\ toMetadata file.uris
    ]

instance ToData Cip25MetadataFile where
  toData (Cip25MetadataFile file) = toData $ AssocMap.Map $
    [ "name" /\ toData file.name
    , "mediaType" /\ toData file.mediaType
    , "src" /\ toData file.uris
    ]

instance FromData Cip25MetadataFile where
  fromData contents = do
    name <- lookupKey "name" contents >>= fromData
    mediaType <- lookupKey "mediaType" contents >>= fromData
    uris <- lookupKey "src" contents >>= fromData
    pure $ wrap { name, mediaType, uris }

instance DecodeJson Cip25MetadataFile where
  decodeJson =
    Json.caseJsonObject errExpectedObject $ \obj -> do
      name <- obj .: "name"
      mediaType <- obj .: "mediaType"
      uris <- decodeNonEmptyStringArray =<< obj .: "src"
      pure $ wrap { name, mediaType, uris }

--------------------------------------------------------------------------------
-- Cip25MetadataEntry
--------------------------------------------------------------------------------

newtype Cip25MetadataEntry = Cip25MetadataEntry
  { policyId :: MintingPolicyHash
  , assetName :: TokenName
  , imageUris :: NonEmpty Array String
  , mediaType :: Maybe String
  , description :: Array String
  , files :: Array Cip25MetadataFile
  }

derive instance Generic Cip25MetadataEntry _
derive instance Newtype Cip25MetadataEntry _
derive instance Eq Cip25MetadataEntry

instance Show Cip25MetadataEntry where
  show = genericShow

metadataEntryToData :: Cip25MetadataEntry -> PlutusData
metadataEntryToData (Cip25MetadataEntry entry) = toData $ AssocMap.Map $
  [ "name" /\ toData entry.assetName
  , "image" /\ toData entry.imageUris
  , "mediaType" /\ toData entry.mediaType
  , "description" /\ toData entry.description
  , "files" /\ toData entry.files
  ]

metadataEntryToMetadata :: Cip25MetadataEntry -> TransactionMetadatum
metadataEntryToMetadata (Cip25MetadataEntry entry) = toMetadata
  [ "name" /\ anyToMetadata entry.assetName
  , "image" /\ anyToMetadata entry.imageUris
  , "mediaType" /\ anyToMetadata entry.mediaType
  , "description" /\ anyToMetadata entry.description
  , "files" /\ anyToMetadata entry.files
  ]

metadataEntryFromData
  :: MintingPolicyHash
  -> TokenName
  -> PlutusData
  -> Maybe Cip25MetadataEntry
metadataEntryFromData policyId assetName contents = do
  imageUris <- lookupKey "image" contents >>= fromData
  mediaType <- lookupKey "mediaType" contents >>= fromData
  description <- lookupKey "description" contents >>= fromData
  files <- lookupKey "files" contents >>= fromData
  pure $
    wrap { policyId, assetName, imageUris, mediaType, description, files }

metadataEntryDecodeJson
  :: MintingPolicyHash
  -> TokenName
  -> Json
  -> Either Json.JsonDecodeError Cip25MetadataEntry
metadataEntryDecodeJson policyId assetName =
  Json.caseJsonObject errExpectedObject $ \obj -> do
    imageUris <- obj .: "image" >>=
      decodeNonEmptyStringArray
    mediaType <- obj .:? "mediaType"
    description <- obj .:? "description" >>=
      maybe (pure mempty) decodeStringArray
    files <- obj .:? "files" >>= \arr ->
      maybe (pure mempty) (traverse Json.decodeJson) (Json.toArray =<< arr)
    pure $
      wrap { policyId, assetName, imageUris, mediaType, description, files }

--------------------------------------------------------------------------------
-- Cip25Metadata
--------------------------------------------------------------------------------

newtype Cip25Metadata = Cip25Metadata (Array Cip25MetadataEntry)

derive instance Generic Cip25Metadata _
derive instance Newtype Cip25Metadata _
derive instance Eq Cip25Metadata

instance Show Cip25Metadata where
  show = genericShow

groupEntries
  :: Array Cip25MetadataEntry -> Array (NonEmptyArray Cip25MetadataEntry)
groupEntries =
  groupBy \(Cip25MetadataEntry a) (Cip25MetadataEntry b) ->
    a.policyId == b.policyId

instance ToMetadata Cip25Metadata where
  toMetadata (Cip25Metadata entries) = toMetadata $
    groupEntries entries <#>
      \group ->
        (_.policyId <<< unwrap $ NonEmpty.head group) /\
          (toArray <<< flip map group) \entry ->
            (unwrap entry).assetName /\ metadataEntryToMetadata entry

instance ToGeneralTransactionMetadata Cip25Metadata where
  metadataLabel _ = wrap (fromInt 721)

instance ToData Cip25Metadata where
  toData (Cip25Metadata entries) = toData
    $ AssocMap.singleton (toData nftMetadataLabel)
    $ AssocMap.Map
    $ groupEntries entries <#>
        \group ->
          toData (_.policyId <<< unwrap $ NonEmpty.head group) /\
            (AssocMap.Map <<< toArray <<< flip map group) \entry ->
              toData ((unwrap entry).assetName) /\ metadataEntryToData entry

instance FromData Cip25Metadata where
  fromData meta = do
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

instance DecodeJson Cip25Metadata where
  decodeJson =
    Json.caseJsonObject errExpectedObject $ \obj -> do
      policies <- obj .: nftMetadataLabel
      withJsonObject policies $ \objPolicies ->
        map (wrap <<< concat)
          $ for (objToArray objPolicies)
          $ \(policyId /\ assets) ->
              withJsonObject assets $ \objAssets ->
                for (objToArray objAssets) $ \(assetName /\ contents) -> do
                  policyId_ <- decodePolicyId policyId
                  assetName_ <- decodeAssetName assetName
                  metadataEntryDecodeJson policyId_ assetName_ contents
    where
    objToArray :: forall a. FO.Object a -> Array (Tuple String a)
    objToArray = FO.toUnfoldable

    withJsonObject
      :: forall a
       . Json
      -> (FO.Object Json -> Either Json.JsonDecodeError a)
      -> Either Json.JsonDecodeError a
    withJsonObject = flip (Json.caseJsonObject errExpectedObject)

    decodePolicyId :: String -> Either Json.JsonDecodeError MintingPolicyHash
    decodePolicyId =
      note (Json.TypeMismatch "Expected hex-encoded policy id")
        <<< map wrap
        <<< (scriptHashFromBytes <=< hexToByteArray)

    decodeAssetName :: String -> Either Json.JsonDecodeError TokenName
    decodeAssetName =
      note (Json.TypeMismatch "Expected UTF-8 encoded asset name")
        <<< mkTokenName
        <<< wrap
        <<< encodeUtf8

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

decodeStringArray
  :: Json -> Either Json.JsonDecodeError (Array String)
decodeStringArray json
  | Json.isString json =
      decodeStringArray (Json.jsonSingletonArray json)
  | otherwise =
      flip (Json.caseJsonArray errExpectedArray) json $
        note (Json.TypeMismatch "Expected UTF-8 encoded string")
          <<< traverse Json.toString

decodeNonEmptyStringArray
  :: Json -> Either Json.JsonDecodeError (NonEmpty Array String)
decodeNonEmptyStringArray json =
  decodeStringArray json >>= \arr ->
    case uncons arr of
      Just { head, tail } -> pure (head :| tail)
      Nothing -> errExpectedNonEmptyArray
