module Cardano.Types.VRFKeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor, decodeCbor)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData (PlutusData(Constr))
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Helpers (compareViaCslBytes, eqOrd)
import Ctl.Internal.ToData (class ToData, toData)
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Either (Either(Left, Right))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype VRFKeyHash = VRFKeyHash Csl.VRFKeyHash

derive instance Newtype VRFKeyHash _
derive instance Generic VRFKeyHash _

instance Show VRFKeyHash where
  show = unwrap >>> toBytes >>> byteArrayToHex

instance Eq VRFKeyHash where
  eq = eqOrd

instance Ord VRFKeyHash where
  compare = compareViaCslBytes `on` unwrap

instance FromData VRFKeyHash where
  fromData (Constr n [ bytes ])
    | n == BigNum.zero = VRFKeyHash <$>
        (fromBytes =<< fromData bytes)
  fromData _ = Nothing

instance ToData VRFKeyHash where
  toData (VRFKeyHash th) = Constr BigNum.zero [ toData $ toBytes th ]

instance EncodeAeson VRFKeyHash where
  encodeAeson = unwrap >>> toBytes >>> byteArrayToHex >>> encodeAeson

instance DecodeAeson VRFKeyHash where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded VRFKeyHash") Right <<<
      caseAesonString Nothing
        (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance AsCbor VRFKeyHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

-- TODO: DecodeAeson
