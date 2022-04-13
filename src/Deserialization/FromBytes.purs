module Deserialization.FromBytes
  ( class FromBytes
  , fromBytes
  , fromBytesEffect
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Exception (throw)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Types
  ( DataHash
  , Mint
  , NativeScript
  , PlutusData
  , Transaction
  , TransactionHash
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , VRFKeyHash
  )
import Types.ByteArray (ByteArray)

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes :: ByteArray -> Maybe a

instance FromBytes DataHash where
  fromBytes = _fromBytesDataHash maybeFfiHelper

instance FromBytes Transaction where
  fromBytes = _fromBytesTransaction maybeFfiHelper

instance FromBytes TransactionHash where
  fromBytes = _fromBytesTransactionHash maybeFfiHelper

instance FromBytes PlutusData where
  fromBytes = _fromBytesPlutusData maybeFfiHelper

instance FromBytes TransactionUnspentOutput where
  fromBytes = _fromBytesTransactionUnspentOutput maybeFfiHelper

instance FromBytes TransactionWitnessSet where
  fromBytes = _fromBytesTransactionWitnessSet maybeFfiHelper

instance FromBytes NativeScript where
  fromBytes = _fromBytesNativeScript maybeFfiHelper

instance FromBytes Mint where
  fromBytes = _fromBytesMint maybeFfiHelper

instance FromBytes VRFKeyHash where
  fromBytes = _fromBytesVRFKeyHash maybeFfiHelper

fromBytesEffect :: forall (a :: Type). FromBytes a => ByteArray -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

foreign import _fromBytesDataHash
  :: MaybeFfiHelper -> ByteArray -> Maybe DataHash

foreign import _fromBytesTransactionHash
  :: MaybeFfiHelper -> ByteArray -> Maybe TransactionHash

foreign import _fromBytesPlutusData
  :: MaybeFfiHelper -> ByteArray -> Maybe PlutusData

foreign import _fromBytesTransaction
  :: MaybeFfiHelper -> ByteArray -> Maybe Transaction

foreign import _fromBytesTransactionUnspentOutput
  :: MaybeFfiHelper -> ByteArray -> Maybe TransactionUnspentOutput

foreign import _fromBytesTransactionWitnessSet
  :: MaybeFfiHelper -> ByteArray -> Maybe TransactionWitnessSet

foreign import _fromBytesNativeScript
  :: MaybeFfiHelper -> ByteArray -> Maybe NativeScript

foreign import _fromBytesMint
  :: MaybeFfiHelper -> ByteArray -> Maybe Mint

foreign import _fromBytesVRFKeyHash
  :: MaybeFfiHelper -> ByteArray -> Maybe VRFKeyHash
