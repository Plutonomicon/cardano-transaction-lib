-- Implementation of CIP 25 - NFT Metadata Standard V2.
-- https://cips.cardano.org/cips/cip25/
-- https://cips.cardano.org/cips/cip25/cddl/version_2.cddl
--
-- Differences from the spec:
-- - We do not split strings in pieces when encoding to JSON
-- - We require a "version": 2 tag.
-- - `policy_id` is 28 bytes
-- - `asset_name` is up to 32 bytes.
--
-- Motivation: https://github.com/cardano-foundation/CIPs/issues/303
module Ctl.Internal.Metadata.Cip25.V2
  ( Cip25Metadata(Cip25Metadata)
  , Cip25MetadataEntry(Cip25MetadataEntry)
  , module Ctl.Internal.Metadata.Cip25.Common
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , (.:)
  , (.:?)
  )
import Aeson as Aeson
import Control.Alt ((<|>))
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Metadata.Cip25.Cip25String
  ( Cip25String
  , fromDataString
  , fromMetadataString
  , toDataString
  , toMetadataString
  )
import Ctl.Internal.Metadata.Cip25.Common
  ( Cip25MetadataFile(Cip25MetadataFile)
  , Cip25TokenName(Cip25TokenName)
  , nftMetadataLabel
  )
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Ctl.Internal.Metadata.Helpers
  ( errExpectedObject
  , lookupKey
  , lookupMetadata
  )
import Ctl.Internal.Metadata.MetadataType (class MetadataType)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Plutus.Types.AssocMap (Map(Map), singleton) as AssocMap
import Ctl.Internal.Serialization.Hash (scriptHashFromBytes, scriptHashToBytes)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.PlutusData (PlutusData(Map, Integer))
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts (MintingPolicyHash)
import Ctl.Internal.Types.TokenName (getTokenName, mkTokenName)
import Ctl.Internal.Types.TransactionMetadata
  ( TransactionMetadatum(Int, MetadataMap)
  )
import Data.Array (catMaybes, concat, groupBy)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Array.NonEmpty (head) as NonEmpty
import Data.Either (Either(Left), note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.TextEncoder (encodeUtf8)
import Data.Traversable (fold, for, sequence, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object, toUnfoldable) as FO
import JS.BigInt (fromInt, toString) as BigInt
import Partial.Unsafe (unsafePartial)

-- | ```
-- | metadata_details =
-- |   {
-- |     name : string,
-- |     image : string / [* string],
-- |     ? mediaType : string,
-- |     ? description : string / [* string],
-- |     ? files : [* files_details]
-- |   }
-- | ```
newtype Cip25MetadataEntry = Cip25MetadataEntry
  { policyId :: MintingPolicyHash
  , assetName :: Cip25TokenName
  -- metadata_details:
  , name :: Cip25String
  , image :: String
  , mediaType :: Maybe Cip25String
  , description :: Maybe String
  , files :: Array Cip25MetadataFile
  }

derive instance Generic Cip25MetadataEntry _
derive instance Newtype Cip25MetadataEntry _
derive instance Eq Cip25MetadataEntry

instance Show Cip25MetadataEntry where
  show = genericShow

-- | A version tag (not exported, internal)
data Cip25V2 = Cip25V2

derive instance Eq Cip25V2
derive instance Ord Cip25V2
derive instance Generic Cip25V2 _

instance Show Cip25V2 where
  show = genericShow

instance FromData Cip25V2 where
  fromData (Integer int)
    | int == BigInt.fromInt 2 = pure Cip25V2
  fromData _ = Nothing

instance ToData Cip25V2 where
  toData _ = Integer $ BigInt.fromInt 2

instance FromMetadata Cip25V2 where
  fromMetadata (Int int)
    | Int.toBigInt int == BigInt.fromInt 2 = pure Cip25V2
  fromMetadata _ = Nothing

instance ToMetadata Cip25V2 where
  toMetadata _ = Int $ unsafePartial $ fromJust $ Int.fromBigInt $
    BigInt.fromInt 2

instance DecodeAeson Cip25V2 where
  decodeAeson aeson = do
    n :: Int <- decodeAeson aeson
    case n of
      2 -> pure Cip25V2
      _ -> Left $ TypeMismatch "Cip25V2"

-- Note: this is not an instance of `ToMetadata` to prevent confusion
-- between `Cip25Metadata` and `Cip25MetadataEntry` (the users do not
-- need to deal with data-representations of a single entry, because
-- the standard only specifies the encoding for Cip25Metadata).
metadataEntryToMetadata :: Cip25MetadataEntry -> TransactionMetadatum
metadataEntryToMetadata (Cip25MetadataEntry entry) = toMetadata $
  [ "name" /\ toMetadata entry.name
  , "image" /\ toMetadataString entry.image
  ]
    <> mbMediaType
    <> mbDescription
    <> mbFiles
  where
  mbFiles = case entry.files of
    [] -> []
    files -> [ "files" /\ toMetadata files ]
  mbMediaType = fold $ entry.mediaType <#> \mediaType ->
    [ "mediaType" /\ toMetadata mediaType ]
  mbDescription = fold $ entry.description <#> \description ->
    [ "description" /\ toMetadataString description ]

metadataEntryFromMetadata
  :: MintingPolicyHash
  -> Cip25TokenName
  -> TransactionMetadatum
  -> Maybe Cip25MetadataEntry
metadataEntryFromMetadata policyId assetName contents = do
  name <- lookupMetadata "name" contents >>= fromMetadata
  image <- lookupMetadata "image" contents >>= fromMetadataString
  mediaType <- for (lookupMetadata "mediaType" contents) fromMetadata
  description <- for (lookupMetadata "description" contents) fromMetadataString
  files <- for (lookupMetadata "files" contents) fromMetadata <#> fromMaybe []
  pure $
    wrap { policyId, assetName, name, image, mediaType, description, files }

metadataEntryToData :: Cip25MetadataEntry -> PlutusData
metadataEntryToData (Cip25MetadataEntry entry) = toData $ AssocMap.Map $
  [ "name" /\ toData entry.name
  , "image" /\ toDataString entry.image
  ]
    <> mbMediaType
    <> mbDescription
    <> mbFiles
  where
  mbFiles = case entry.files of
    [] -> []
    files -> [ "files" /\ toData files ]
  mbMediaType = fold $ entry.mediaType <#> \mediaType ->
    [ "mediaType" /\ toData mediaType ]
  mbDescription = fold $ entry.description <#> \description ->
    [ "description" /\ toDataString description ]

metadataEntryFromData
  :: MintingPolicyHash
  -> Cip25TokenName
  -> PlutusData
  -> Maybe Cip25MetadataEntry
metadataEntryFromData policyId assetName contents = do
  name <- lookupKey "name" contents >>= fromData
  image <- lookupKey "image" contents >>= fromDataString
  mediaType <- for (lookupKey "mediaType" contents) fromData
  description <- for (lookupKey "description" contents) fromDataString
  files <- for (lookupKey "files" contents) fromData <#> fromMaybe []
  pure $
    wrap { policyId, assetName, name, image, mediaType, description, files }

metadataEntryDecodeAeson
  :: MintingPolicyHash
  -> Cip25TokenName
  -> Aeson
  -> Either JsonDecodeError Cip25MetadataEntry
metadataEntryDecodeAeson policyId assetName =
  caseAesonObject errExpectedObject \obj -> do
    name <- obj .: "name"
    image <- obj .: "image"
    mediaType <- obj .:? "mediaType"
    description <- obj .:? "description" <#> fromMaybe mempty
    files <- obj .:? "files" >>= \mbFiles ->
      fromMaybe [] <$> for mbFiles \files ->
        traverse decodeAeson =<< note (TypeMismatch "files")
          (Aeson.toArray files)
    pure $
      wrap { policyId, assetName, name, image, mediaType, description, files }

-- | Encode the entry's policy id to the string used as the metadata
-- | key
encodePolicyIdKey :: Cip25MetadataEntry -> String
encodePolicyIdKey (Cip25MetadataEntry { policyId }) =
  rawBytesToHex $ scriptHashToBytes $ unwrap policyId

-- | Decode the CIP25 policy id key
decodePolicyIdKey :: String -> Maybe MintingPolicyHash
decodePolicyIdKey = map wrap <<< scriptHashFromBytes <=< hexToByteArray

-- | Encode the entry's asset name to the string used as the metadata
-- | key
encodeAssetNameKey :: Cip25MetadataEntry -> String
encodeAssetNameKey (Cip25MetadataEntry { assetName }) =
  byteArrayToHex $ getTokenName $ unwrap assetName

-- | Decode the CIP25 asset name key
decodeAssetNameKey :: String -> Maybe Cip25TokenName
decodeAssetNameKey = map wrap <<< mkTokenName <=< hexToByteArray

newtype Cip25Metadata = Cip25Metadata (Array Cip25MetadataEntry)

derive instance Generic Cip25Metadata _
derive instance Newtype Cip25Metadata _
derive instance Eq Cip25Metadata

instance Show Cip25Metadata where
  show = genericShow

instance MetadataType Cip25Metadata where
  metadataLabel _ = wrap nftMetadataLabel

groupEntries
  :: Array Cip25MetadataEntry -> Array (NonEmptyArray Cip25MetadataEntry)
groupEntries = groupBy (eq `on` (unwrap >>> _.policyId))

instance ToMetadata Cip25Metadata where
  toMetadata (Cip25Metadata entries) = toMetadata $
    let
      dataEntries =
        groupEntries entries <#>
          \group ->
            ( toMetadata $ encodePolicyIdKey $ NonEmpty.head group
            ) /\
              (toMetadata <<< toArray <<< flip map group) \entry ->
                (encodeAssetNameKey entry) /\ metadataEntryToMetadata entry
      versionEntry = [ toMetadata "version" /\ toMetadata Cip25V2 ]
    in
      dataEntries <> versionEntry

instance FromMetadata Cip25Metadata where
  fromMetadata (MetadataMap mp1) = do
    entries <- map concat
      $ map catMaybes
      $ for (Map.toUnfoldable mp1)
          \(key /\ assets) ->
            if key == toMetadata "version" then
              -- top-level version tag
              ( if assets == toMetadata Cip25V2 then pure Nothing
                else Nothing -- fail if version does not match
              )
            else
              -- key is policyId
              Just case assets of
                MetadataMap mp2 ->
                  for (Map.toUnfoldable mp2) \(assetName /\ contents) ->
                    metadataEntryFromMetadata
                      <$> (decodePolicyIdKey =<< fromMetadata key)
                      <*> (decodeAssetNameKey =<< fromMetadata assetName)
                      <*> pure contents
                _ -> Nothing
    wrap <$> sequence entries
  fromMetadata _ = Nothing

instance ToData Cip25Metadata where
  toData (Cip25Metadata entries) = toData
    $ AssocMap.singleton (toData $ BigInt.toString nftMetadataLabel)
    $ AssocMap.Map
        let
          dataEntries =
            groupEntries entries <#>
              \group ->
                toData (encodePolicyIdKey $ NonEmpty.head group) /\ toData
                  ( (AssocMap.Map <<< toArray <<< flip map group) \entry ->
                      toData (encodeAssetNameKey entry) /\ metadataEntryToData
                        entry
                  )
          versionEntry = [ toData "version" /\ toData Cip25V2 ]
        in
          dataEntries <> versionEntry

instance FromData Cip25Metadata where
  fromData meta = do
    entries <- lookupKey (BigInt.toString nftMetadataLabel) meta >>= case _ of
      Map mp1 -> map concat
        $ for mp1
            \(policyId /\ assets) ->
              let
                fromDataAssets =
                  case assets of
                    Map mp2 ->
                      for mp2 \(assetName /\ contents) ->
                        metadataEntryFromData
                          <$> (decodePolicyIdKey =<< fromData policyId)
                          <*> (decodeAssetNameKey =<< fromData assetName)
                          <*> pure contents
                    _ -> Nothing
                fromDataVersion =
                  if
                    policyId == toData "version"
                      && assets == toData Cip25V2 then
                    pure []
                  else
                    Nothing
              in
                fromDataAssets <|> fromDataVersion
      _ -> Nothing
    wrap <$> sequence entries

instance DecodeAeson Cip25Metadata where
  decodeAeson =
    caseAesonObject errExpectedObject \obj -> do
      policies <- obj .: BigInt.toString nftMetadataLabel
      withJsonObject policies \objPolicies ->
        map (wrap <<< concat)
          $ for (objToArray objPolicies)
              \(policyId /\ assets) ->
                withJsonObject assets \objAssets ->
                  for (objToArray objAssets) \(assetName /\ contents) -> do
                    policyId_ <- decodePolicyId policyId
                    assetName_ <- decodeAssetName assetName
                    metadataEntryDecodeAeson policyId_ assetName_ contents
    where
    objToArray :: forall (a :: Type). FO.Object a -> Array (Tuple String a)
    objToArray = FO.toUnfoldable

    withJsonObject
      :: forall (a :: Type)
       . Aeson
      -> (FO.Object Aeson -> Either JsonDecodeError a)
      -> Either JsonDecodeError a
    withJsonObject = flip (caseAesonObject errExpectedObject)

    decodePolicyId :: String -> Either JsonDecodeError MintingPolicyHash
    decodePolicyId =
      note (TypeMismatch "Expected hex-encoded policy id")
        <<< map wrap
        <<< (scriptHashFromBytes <=< hexToByteArray)

    decodeAssetName :: String -> Either JsonDecodeError Cip25TokenName
    decodeAssetName =
      note (TypeMismatch "Expected UTF-8 encoded asset name")
        <<< map wrap
        <<< mkTokenName
        <<< wrap
        <<< encodeUtf8
