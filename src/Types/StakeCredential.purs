module Cardano.Types.StakeCredential where
import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(..), caseAesonString, encodeAeson)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap, wrap)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)

newtype StakeCredential = StakeCredential Csl.StakeCredential

derive instance Newtype StakeCredential _
derive instance Generic StakeCredential _

instance AsCbor StakeCredential where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

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
      caseAesonString Nothing (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson StakeCredential where
  encodeAeson sh = encodeAeson $ encodeCbor sh
