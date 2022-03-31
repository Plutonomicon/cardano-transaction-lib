module Metadata.Seabug
  ( SeabugMetadata(SeabugMetadata)
  , SeabugMetadataDelta(SeabugMetadataDelta)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Json
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import FromData (class FromData, fromData)
import Metadata.Seabug.Share (Share, mkShare)
import Partial.Unsafe (unsafePartial)
import ToData (class ToData, toData)
import Types.ByteArray (ByteArray, byteArrayFromString)
import Types.Natural (Natural)
import Types.PlutusData (PlutusData(Bytes, Map))
import Types.Scripts (MintingPolicyHash, ValidatorHash)
import Types.UnbalancedTransaction (PubKeyHash)
import Types.Value (CurrencySymbol, TokenName, mkTokenName)

newtype SeabugMetadata = SeabugMetadata
  { policyId :: MintingPolicyHash
  , mintPolicy :: ByteArray
  , collectionNftCS :: CurrencySymbol
  , collectionNftTN :: TokenName
  , lockingScript :: ValidatorHash
  , authorPkh :: PubKeyHash
  , authorShare :: Share
  , marketplaceScript :: ValidatorHash
  , marketplaceShare :: Share
  , ownerPkh :: PubKeyHash
  , ownerPrice :: Natural
  }

derive instance Generic SeabugMetadata _
derive instance Newtype SeabugMetadata _
derive instance Eq SeabugMetadata

instance Show SeabugMetadata where
  show = genericShow

instance ToData SeabugMetadata where
  toData (SeabugMetadata meta) = unsafePartial $ toData $ Map.fromFoldable
    [ mkKey "727" /\ Map.fromFoldable
        [ meta.policyId /\ Map.fromFoldable
            [ mkKey "mintPolicy" /\ toData meta.mintPolicy
            , mkKey "collectionNftCS" /\ toData meta.collectionNftCS
            , mkKey "collectionNftTN" /\ toData meta.collectionNftTN
            , mkKey "lockingScript" /\ toData meta.lockingScript
            , mkKey "authorPkh" /\ toData meta.authorPkh
            , mkKey "authorShare" /\ toData meta.authorShare
            , mkKey "marketplaceScript" /\ toData meta.marketplaceScript
            , mkKey "marketplaceShare" /\ toData meta.marketplaceShare
            , mkKey "ownerPkh" /\ toData meta.ownerPkh
            , mkKey "ownerPrice" /\ toData meta.ownerPrice
            ]
        ]
    ]

instance FromData SeabugMetadata where
  fromData (Map sm) = unsafePartial do
    policyId /\ contents <- lookupKey "727" sm >>= case _ of
      Map mp1 -> case Map.toUnfoldable mp1 of
        [ policyId /\ contents ] -> Tuple <$> fromData policyId <*> fromData contents
        _ -> Nothing
      _ -> Nothing
    mintPolicy <- lookupKey "mintPolicy" contents >>= fromData
    collectionNftCS <- lookupKey "collectionNftCS" contents >>= fromData
    collectionNftTN <- lookupKey "collectionNftTN" contents >>= fromData
    lockingScript <- lookupKey "lockingScript" contents >>= fromData
    authorPkh <- lookupKey "authorPkh" contents >>= fromData
    authorShare <- lookupKey "authorShare" contents >>= fromData
    marketplaceScript <- lookupKey "marketplaceScript" contents >>= fromData
    marketplaceShare <- lookupKey "marketplaceShare" contents >>= fromData
    ownerPkh <- lookupKey "ownerPkh" contents >>= fromData
    ownerPrice <- lookupKey "ownerPrice" contents >>= fromData
    pure $ SeabugMetadata
      { policyId
      , mintPolicy
      , collectionNftCS
      , collectionNftTN
      , lockingScript
      , authorPkh
      , authorShare
      , marketplaceScript
      , marketplaceShare
      , ownerPkh
      , ownerPrice
      }
  fromData _ = Nothing

instance DecodeJson SeabugMetadata where
  decodeJson =
    Json.caseJsonObject
      (Left (Json.TypeMismatch "Expected object"))
      $ \o -> do
          policyId <- Json.getField o "policyId"
          mintPolicy <- Json.getField o "mintPolicy"
          collectionNftCS <- Json.getField o "collectionNftCS"
          collectionNftTN <-
            note (Json.TypeMismatch "expected ASCII-encoded `TokenName`")
              <<< (mkTokenName <=< byteArrayFromString)
              =<< Json.getField o "collectionNftTN"
          lockingScript <- Json.getField o "lockingScript"
          authorPkh <- Json.getField o "authorPkh"
          authorShare <- decodeShare =<< Json.getField o "authorShare"
          marketplaceScript <- Json.getField o "marketplaceScript"
          marketplaceShare <- decodeShare =<< Json.getField o "marketplaceShare"
          ownerPkh <- Json.getField o "ownerPkh"
          ownerPrice <- Json.getField o "ownerPrice"
          pure $ SeabugMetadata
            { policyId
            , mintPolicy
            , collectionNftCS
            , collectionNftTN
            , lockingScript
            , authorPkh
            , authorShare
            , marketplaceScript
            , marketplaceShare
            , ownerPkh
            , ownerPrice
            }
    where
    decodeShare :: Int -> Either Json.JsonDecodeError Share
    decodeShare = note (Json.TypeMismatch "Expected int between 0 and 1000")
      <<< mkShare

newtype SeabugMetadataDelta = SeabugMetadataDelta
  { policyId :: MintingPolicyHash
  , ownerPkh :: PubKeyHash
  , ownerPrice :: Natural
  }

derive instance Generic SeabugMetadataDelta _
derive instance Newtype SeabugMetadataDelta _
derive instance Eq SeabugMetadataDelta

instance Show SeabugMetadataDelta where
  show = genericShow

instance ToData SeabugMetadataDelta where
  toData (SeabugMetadataDelta meta) = unsafePartial $ toData $ Map.fromFoldable
    [ mkKey "727" /\ Map.fromFoldable
        [ meta.policyId /\ Map.fromFoldable
            [ mkKey "ownerPkh" /\ toData meta.ownerPkh
            , mkKey "ownerPrice" /\ toData meta.ownerPrice
            ]
        ]
    ]

instance FromData SeabugMetadataDelta where
  fromData (Map sm) = unsafePartial do
    policyId /\ contents <- lookupKey "727" sm >>= case _ of
      Map mp1 -> case Map.toUnfoldable mp1 of
        [ policyId /\ contents ] -> Tuple <$> fromData policyId <*> fromData contents
        _ -> Nothing
      _ -> Nothing
    ownerPkh <- lookupKey "ownerPkh" contents >>= fromData
    ownerPrice <- lookupKey "ownerPrice" contents >>= fromData
    pure $ SeabugMetadataDelta
      { policyId
      , ownerPkh
      , ownerPrice
      }
  fromData _ = Nothing

mkKey :: Partial => String -> PlutusData
mkKey str = Bytes $ fromJust $ byteArrayFromString str

lookupKey :: Partial => String -> Map.Map PlutusData PlutusData -> Maybe PlutusData
lookupKey keyStr = Map.lookup (mkKey keyStr)
