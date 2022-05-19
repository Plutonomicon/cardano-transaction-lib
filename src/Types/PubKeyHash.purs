module Types.PubKeyHash
  ( PubKeyHash(PubKeyHash)
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , caseJsonObject
  , decodeJson
  , getField
  , JsonDecodeError(TypeMismatch)
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Hash (Ed25519KeyHash)
import ToData (class ToData)
import Aeson (class DecodeAeson, class EncodeAeson)
import Data.Newtype (class Newtype, unwrap, wrap)
import Record (get)
import Type.Proxy (Proxy(Proxy))
import Aeson.Decode as D
import Aeson.Encode as E

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

-- This is needed for `ApplyArgs`. Plutus has an `getPubKeyHash` field so don't
-- newtype derive.
instance DecodeJson PubKeyHash where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getPubKeyHash" >=> decodeJson >>> map PubKeyHash)

-- NOTE: mlabs-haskell/purescript-bridge generated and applied here
instance EncodeAeson PubKeyHash where
  encodeAeson' x = pure $ E.encode
    (E.record { getPubKeyHash: E.value :: _ (Ed25519KeyHash) })
    { getPubKeyHash: unwrap x }

instance DecodeAeson PubKeyHash where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getPubKeyHash") <$> D.decode
    (D.record "getPubKeyHash " { getPubKeyHash: D.value :: _ (Ed25519KeyHash) })
    x
