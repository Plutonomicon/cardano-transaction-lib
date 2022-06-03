module Metadata.Cip25
  ( Cip25Metadata(Cip25Metadata)
  , Cip25MetadataEntry(Cip25MetadataEntry)
  , Cip25MetadataFile(Cip25MetadataFile)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError
      ( TypeMismatch
      )
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , isString
  , toString
  , (.:)
  , (.:?)
  )
import Aeson as Aeson
import Data.Array (concat, groupBy, uncons)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Array.NonEmpty (head) as NonEmpty
import Data.BigInt (fromInt) as BigInt
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
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
import Metadata.Helpers (lookupKey, lookupMetadata)
import Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Metadata.ToMetadata (class ToMetadata, toMetadata, anyToMetadata)
import Metadata.MetadataType (class MetadataType)
import Serialization.Hash (scriptHashFromBytes)
import Types.Scripts (MintingPolicyHash)
import Types.RawBytes (hexToRawBytes)
import Types.PlutusData (PlutusData(Map))
import Types.TokenName (TokenName, mkTokenName)
import Types.TransactionMetadata (TransactionMetadatum(MetadataMap))

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

instance FromMetadata Cip25MetadataFile where
  fromMetadata contents = do
    name <- lookupMetadata "name" contents >>= fromMetadata
    mediaType <- lookupMetadata "mediaType" contents >>= fromMetadata
    uris <- lookupMetadata "src" contents >>= fromMetadata
    pure $ wrap { name, mediaType, uris }

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

instance DecodeAeson Cip25MetadataFile where
  decodeAeson =
    caseAesonObject errExpectedObject $ \obj -> do
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

metadataEntryToMetadata :: Cip25MetadataEntry -> TransactionMetadatum
metadataEntryToMetadata (Cip25MetadataEntry entry) = toMetadata
  [ "name" /\ anyToMetadata entry.assetName
  , "image" /\ anyToMetadata entry.imageUris
  , "mediaType" /\ anyToMetadata entry.mediaType
  , "description" /\ anyToMetadata entry.description
  , "files" /\ anyToMetadata entry.files
  ]

metadataEntryFromMetadata
  :: MintingPolicyHash
  -> TokenName
  -> TransactionMetadatum
  -> Maybe Cip25MetadataEntry
metadataEntryFromMetadata policyId assetName contents = do
  imageUris <- lookupMetadata "image" contents >>= fromMetadata
  let mediaType = lookupMetadata "mediaType" contents >>= fromMetadata
  description <- lookupMetadata "description" contents >>= fromMetadata
  files <- lookupMetadata "files" contents >>= fromMetadata
  pure $
    wrap { policyId, assetName, imageUris, mediaType, description, files }

metadataEntryToData :: Cip25MetadataEntry -> PlutusData
metadataEntryToData (Cip25MetadataEntry entry) = toData $ AssocMap.Map $
  [ "name" /\ toData entry.assetName
  , "image" /\ toData entry.imageUris
  , "mediaType" /\ toData entry.mediaType
  , "description" /\ toData entry.description
  , "files" /\ toData entry.files
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

metadataEntryDecodeAeson
  :: MintingPolicyHash
  -> TokenName
  -> Aeson
  -> Either JsonDecodeError Cip25MetadataEntry
metadataEntryDecodeAeson policyId assetName =
  caseAesonObject errExpectedObject $ \obj -> do
    imageUris <- obj .: "image" >>=
      decodeNonEmptyStringArray
    mediaType <- obj .:? "mediaType"
    description <- obj .:? "description" >>=
      maybe (pure mempty) decodeStringArray
    files <- obj .:? "files" >>= \arr ->
      maybe (pure mempty) (traverse decodeAeson) (Aeson.toArray =<< arr)
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

instance MetadataType Cip25Metadata where
  metadataLabel _ = wrap (BigInt.fromInt 721)

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

instance FromMetadata Cip25Metadata where
  fromMetadata (MetadataMap mp1) = do
    entries <- map concat
      $ for (Map.toUnfoldable mp1)
      $ \(policyId /\ assets) ->
          case assets of
            MetadataMap mp2 ->
              for (Map.toUnfoldable mp2) $ \(assetName /\ contents) ->
                metadataEntryFromMetadata <$> fromMetadata policyId
                  <*> fromMetadata assetName
                  <*> pure contents
            _ -> Nothing
    wrap <$> sequence entries
  fromMetadata _ = Nothing

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

instance DecodeAeson Cip25Metadata where
  decodeAeson =
    caseAesonObject errExpectedObject $ \obj -> do
      policies <- obj .: nftMetadataLabel
      withJsonObject policies $ \objPolicies ->
        map (wrap <<< concat)
          $ for (objToArray objPolicies)
          $ \(policyId /\ assets) ->
              withJsonObject assets $ \objAssets ->
                for (objToArray objAssets) $ \(assetName /\ contents) -> do
                  policyId_ <- decodePolicyId policyId
                  assetName_ <- decodeAssetName assetName
                  metadataEntryDecodeAeson policyId_ assetName_ contents
    where
    objToArray :: forall a. FO.Object a -> Array (Tuple String a)
    objToArray = FO.toUnfoldable

    withJsonObject
      :: forall a
       . Aeson
      -> (FO.Object Aeson -> Either JsonDecodeError a)
      -> Either JsonDecodeError a
    withJsonObject = flip (caseAesonObject errExpectedObject)

    decodePolicyId :: String -> Either JsonDecodeError MintingPolicyHash
    decodePolicyId =
      note (TypeMismatch "Expected hex-encoded policy id")
        <<< map wrap
        <<< (scriptHashFromBytes <=< hexToRawBytes)

    decodeAssetName :: String -> Either JsonDecodeError TokenName
    decodeAssetName =
      note (TypeMismatch "Expected UTF-8 encoded asset name")
        <<< mkTokenName
        <<< wrap
        <<< encodeUtf8

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

errExpectedObject :: forall a. Either JsonDecodeError a
errExpectedObject =
  Left (TypeMismatch "Expected object")

errExpectedArray :: forall a. Either JsonDecodeError a
errExpectedArray =
  Left (TypeMismatch "Expected array")

errExpectedNonEmptyArray :: forall a. Either JsonDecodeError a
errExpectedNonEmptyArray =
  Left (TypeMismatch "Expected non-empty array")

decodeStringArray
  :: Aeson -> Either JsonDecodeError (Array String)
decodeStringArray aeson
  | isString aeson =
      decodeStringArray (encodeAeson [ aeson ])
  | otherwise =
      flip (caseAesonArray errExpectedArray) aeson $
        note (TypeMismatch "Expected UTF-8 encoded string")
          <<< traverse toString

decodeNonEmptyStringArray
  :: Aeson -> Either JsonDecodeError (NonEmpty Array String)
decodeNonEmptyStringArray json =
  decodeStringArray json >>= \arr ->
    case uncons arr of
      Just { head, tail } -> pure (head :| tail)
      Nothing -> errExpectedNonEmptyArray
