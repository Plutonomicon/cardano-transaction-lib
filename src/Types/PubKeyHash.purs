module Types.PubKeyHash
  ( PubKeyHash(PubKeyHash)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , JsonDecodeError(..)
  , caseAesonObject
  , decodeAeson
  , getField
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Hash (Ed25519KeyHash)
import ToData (class ToData)

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
instance DecodeAeson PubKeyHash where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getPubKeyHash" >=> decodeAeson >>> map PubKeyHash)
