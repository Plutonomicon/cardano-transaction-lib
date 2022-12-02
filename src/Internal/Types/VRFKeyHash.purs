module Ctl.Internal.Types.VRFKeyHash
  ( VRFKeyHash(VRFKeyHash)
  , vrfKeyHashFromBytes
  , vrfKeyHashToBytes
  , unVRFKeyHash
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson')
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Hash as Serialization
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Data.Function (on)
import Data.Maybe (Maybe)
-- TODO: Remove once toBytes is switched to Castable
import Untagged.Union (asOneOf)

newtype VRFKeyHash = VRFKeyHash Serialization.VRFKeyHash

instance Show VRFKeyHash where
  show (VRFKeyHash kh) =
    "(VRFKeyHash " <> show (byteArrayToHex $ toBytes $ asOneOf kh) <> ")"

instance Eq VRFKeyHash where
  eq = eq `on` vrfKeyHashToBytes

instance EncodeAeson VRFKeyHash where
  encodeAeson' (VRFKeyHash kh) =
    asOneOf kh # toBytes >>> byteArrayToHex >>> encodeAeson'

unVRFKeyHash :: VRFKeyHash -> Serialization.VRFKeyHash
unVRFKeyHash (VRFKeyHash kh) = kh

vrfKeyHashFromBytes :: ByteArray -> Maybe VRFKeyHash
vrfKeyHashFromBytes = fromBytes >>> map VRFKeyHash

vrfKeyHashToBytes :: VRFKeyHash -> ByteArray
vrfKeyHashToBytes (VRFKeyHash kh) = toBytes $ asOneOf kh
