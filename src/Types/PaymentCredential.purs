module Cardano.Types.PaymentCredential where
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

-- in CSL, StakeCredential and PaymentCredential are the same type, because they are
-- literally the same, but we treat them differently as domain types
newtype PaymentCredential = PaymentCredential Csl.StakeCredential

derive instance Newtype PaymentCredential _
derive instance Generic PaymentCredential _

instance AsCbor PaymentCredential where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance Eq PaymentCredential where
  eq = eq `on` encodeCbor

instance Ord PaymentCredential where
  compare = compare `on` encodeCbor

instance Show PaymentCredential where
  show = genericShow

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson PaymentCredential where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded PaymentCredential") Right <<<
      caseAesonString Nothing (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson PaymentCredential where
  encodeAeson sh = encodeAeson $ encodeCbor sh
