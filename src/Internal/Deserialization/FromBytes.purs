module Ctl.Internal.Deserialization.FromBytes
  ( class FromBytes
  , FromBytesError
  , _fromBytesError
  , fromBytesError
  , fromBytes'
  , fromBytes
  , fromBytesEffect
  ) where

import Prelude

import Ctl.Internal.Error (E)
import Ctl.Internal.FfiHelpers (ErrorFfiHelper, errorHelper)
import Ctl.Internal.Serialization.Types
  ( DataHash
  , Mint
  , NativeScript
  , PlutusData
  , Transaction
  , TransactionHash
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , VRFKeyHash
  , Value
  )
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Either (Either(Left), hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Exception (throw)
import Type.Prelude (Proxy(Proxy))
import Type.Row (type (+))

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes' :: forall (r :: Row Type). ByteArray -> E (FromBytesError + r) a

instance FromBytes DataHash where
  fromBytes' = _fromBytes "DataHash" eh

instance FromBytes Transaction where
  fromBytes' = _fromBytes "Transaction" eh

instance FromBytes TransactionHash where
  fromBytes' = _fromBytes "TransactionHash" eh

instance FromBytes PlutusData where
  fromBytes' = _fromBytes "PlutusData" eh

instance FromBytes TransactionUnspentOutput where
  fromBytes' = _fromBytes "TransactionUnspentOutput" eh

instance FromBytes TransactionWitnessSet where
  fromBytes' = _fromBytes "TransactionWitnessSet" eh

instance FromBytes NativeScript where
  fromBytes' = _fromBytes "NativeScript" eh

instance FromBytes Mint where
  fromBytes' = _fromBytes "Mint" eh

instance FromBytes VRFKeyHash where
  fromBytes' = _fromBytes "VRFKeyHash" eh

instance FromBytes Value where
  fromBytes' = _fromBytes "Value" eh

-- for backward compatibility until `Maybe` is abandoned. Then to be renamed.
fromBytes :: forall (a :: Type). FromBytes a => ByteArray -> Maybe a
fromBytes = fromBytes' >>> hush

fromBytesEffect :: forall (a :: Type). FromBytes a => ByteArray -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

---- Error types

-- | FromBytesError row alias
type FromBytesError r = (fromBytesError :: String | r)

-- | Needed to craate a variant type
_fromBytesError = Proxy :: Proxy "fromBytesError"

-- | An error to use
fromBytesError
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> E (FromBytesError + r) a
fromBytesError = Left <<< inj _fromBytesError

-- | A local helper to shorten code
eh :: forall (r :: Row Type). ErrorFfiHelper (FromBytesError + r)
eh = errorHelper (inj _fromBytesError)

---- Foreign imports

foreign import _fromBytes
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> ErrorFfiHelper r
  -> ByteArray
  -> E r a
