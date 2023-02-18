module Ctl.Internal.Types.VRFKeyHash
  ( VRFKeyHash(VRFKeyHash)
  , vrfKeyHashFromBytes
  , vrfKeyHashToBytes
  , unVRFKeyHash
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Serialization.Hash as Serialization
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Data.Function (on)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)

newtype VRFKeyHash = VRFKeyHash Serialization.VRFKeyHash

instance Show VRFKeyHash where
  show (VRFKeyHash kh) =
    "(VRFKeyHash " <> show (byteArrayToHex $ unwrap $ toBytes kh) <> ")"

instance Eq VRFKeyHash where
  eq = eq `on` vrfKeyHashToBytes

instance EncodeAeson VRFKeyHash where
  encodeAeson (VRFKeyHash kh) =
    toBytes kh # unwrap >>> byteArrayToHex >>> encodeAeson

unVRFKeyHash :: VRFKeyHash -> Serialization.VRFKeyHash
unVRFKeyHash (VRFKeyHash kh) = kh

vrfKeyHashFromBytes :: ByteArray -> Maybe VRFKeyHash
vrfKeyHashFromBytes = wrap >>> fromBytes >>> map VRFKeyHash

vrfKeyHashToBytes :: VRFKeyHash -> ByteArray
vrfKeyHashToBytes (VRFKeyHash kh) = unwrap $ toBytes kh
