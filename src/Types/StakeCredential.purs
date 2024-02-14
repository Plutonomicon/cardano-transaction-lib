module Cardano.Types.StakeCredential where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(..)
  , caseAesonString
  , encodeAeson
  )
import Cardano.Types.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types.Credential (Credential)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype StakeCredential = StakeCredential Credential

derive instance Newtype StakeCredential _
derive instance Generic StakeCredential _

instance AsCbor StakeCredential where
  encodeCbor = unwrap >>> encodeCbor
  decodeCbor = decodeCbor >>> map wrap

instance Eq StakeCredential where
  eq = eq `on` encodeCbor

instance Ord StakeCredential where
  compare = compare `on` encodeCbor

instance Show StakeCredential where
  show = genericShow

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson StakeCredential where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded StakeCredential") Right <<<
      caseAesonString Nothing
        (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson StakeCredential where
  encodeAeson sh = encodeAeson $ encodeCbor sh
