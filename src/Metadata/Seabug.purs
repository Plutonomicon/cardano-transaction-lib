module Metadata.Seabug
  ( SeabugMetadata(SeabugMetadata)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import FromData (class FromData, fromData)
import Metadata.Seabug.Share (Share)
import Partial.Unsafe (unsafePartial)
import ToData (class ToData, toData)
import Types.ByteArray (byteArrayFromString)
import Types.Natural (Natural)
import Types.PlutusData (PlutusData(Bytes, Map))
import Types.Scripts (MintingPolicyHash, ValidatorHash)
import Types.UnbalancedTransaction (PubKeyHash)
import Types.Value (CurrencySymbol, TokenName)

newtype SeabugMetadata = SeabugMetadata
  { mintPolicy :: MintingPolicyHash
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

derive instance Eq SeabugMetadata

instance Show SeabugMetadata where
  show = genericShow

instance ToData SeabugMetadata where
  toData (SeabugMetadata meta) = unsafePartial $ toData $ Map.fromFoldable
    [ mkKey "727" /\ Map.fromFoldable
        [ meta.mintPolicy /\ Map.fromFoldable
            [ mkKey "collectionNftCS" /\ toData meta.collectionNftCS
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
    mintPolicy /\ contents <- lookupKey "727" sm >>= case _ of
      Map mp1 -> case Map.toUnfoldable mp1 of
        [ mintPolicy /\ contents ] -> Tuple <$> fromData mintPolicy <*> fromData contents
        _ -> Nothing
      _ -> Nothing
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
      { mintPolicy
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

mkKey :: Partial => String -> PlutusData
mkKey str = Bytes $ fromJust $ byteArrayFromString str

lookupKey :: Partial => String -> Map.Map PlutusData PlutusData -> Maybe PlutusData
lookupKey keyStr = Map.lookup (mkKey keyStr)
