module Ctl.Internal.Types.VRFKeyHash
  ( VRFKeyHash(VRFKeyHash)
  , vrfKeyHashFromBytes
  , vrfKeyHashToBytes
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Data.ByteArray (ByteArray, byteArrayToHex)
import Data.Function (on)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype VRFKeyHash = VRFKeyHash Csl.VRFKeyHash

derive instance Newtype VRFKeyHash _

instance Show VRFKeyHash where
  show (VRFKeyHash kh) =
    "(VRFKeyHash " <> show (byteArrayToHex $ toBytes kh) <> ")"

instance Eq VRFKeyHash where
  eq = eq `on` vrfKeyHashToBytes

instance EncodeAeson VRFKeyHash where
  encodeAeson (VRFKeyHash kh) =
    toBytes kh # byteArrayToHex >>> encodeAeson

vrfKeyHashFromBytes :: ByteArray -> Maybe VRFKeyHash
vrfKeyHashFromBytes = fromBytes >>> map VRFKeyHash

vrfKeyHashToBytes :: VRFKeyHash -> ByteArray
vrfKeyHashToBytes (VRFKeyHash kh) = toBytes kh
