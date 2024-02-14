module Cardano.Plutus.Types.PubKeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , (.:)
  )
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Ctl.Internal.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance FromData PubKeyHash
derive newtype instance FromMetadata PubKeyHash
derive newtype instance Ord PubKeyHash
derive newtype instance ToData PubKeyHash
derive newtype instance ToMetadata PubKeyHash

instance Show PubKeyHash where
  show = genericShow

instance EncodeAeson PubKeyHash where
  encodeAeson x = encodeAeson { getPubKeyHash: unwrap x }

instance DecodeAeson PubKeyHash where
  decodeAeson a = do
    obj <- decodeAeson a
    wrap <$> obj .: "getPubKeyHash"
