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
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Serialization.Hash (Ed25519KeyHash)
import ToData (class ToData)

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance FromData PubKeyHash
derive newtype instance Ord PubKeyHash
derive newtype instance ToData PubKeyHash

instance Show PubKeyHash where
  show = genericShow

-- This is needed for `ApplyArgs`. Plutus has an `getPubKeyHash` field so don't
-- newtype derive.
instance DecodeJson PubKeyHash where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getPubKeyHash" >=> decodeJson >>> map PubKeyHash)

-- payPubKeyHash :: PaymentPubKey -> Maybe PaymentPubKeyHash
-- payPubKeyHash (PaymentPubKey pk) = wrap <$> pubKeyHash pk

-- pubKeyHash :: PublicKey -> Maybe PubKeyHash
-- pubKeyHash (PublicKey bech32) =
--   wrap <$> ed25519KeyHashFromBech32 bech32
