module Deserialization.FromBytes
  ( class FromBytes
  , fromBytes
  , fromBytesEffect
  ) where

import Serialization.Types
import Types.ByteArray
import Data.Maybe
import FfiHelpers
import Effect
import Effect.Exception
import Prelude

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes :: ByteArray -> Maybe a


instance FromBytes DataHash where
  fromBytes = _fromBytesDataHash maybeFfiHelper

instance FromBytes TransactionHash where
  fromBytes = _fromBytesTransactionHash maybeFfiHelper

fromBytesEffect :: forall a. FromBytes a => ByteArray -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

foreign import _fromBytesDataHash :: MaybeFfiHelper -> ByteArray -> Maybe DataHash
foreign import _fromBytesTransactionHash :: MaybeFfiHelper -> ByteArray -> Maybe TransactionHash
