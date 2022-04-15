module Deserialization.FromBytes
  ( class FromBytes
  , FromBytesError
  , _fromBytesError
  , fromBytesError
  , fromBytes'
  , fromBytes
  , fromBytesEffect
  ) where

import Prelude

import Contract.Prelude (Either(..), hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Exception (throw)
import Error (E)
import FfiHelpers (ErrorFfiHelper, errorHelper)
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
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Types.ByteArray (ByteArray)

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes' :: forall r. ByteArray -> E (FromBytesError + r) a

instance FromBytes DataHash where
  fromBytes' = _fromBytesDataHash (eh "DataHash")

instance FromBytes Transaction where
  fromBytes' = _fromBytesTransaction (eh "Transaction")

instance FromBytes TransactionHash where
  fromBytes' = _fromBytesTransactionHash (eh "TransactionHash")

instance FromBytes PlutusData where
  fromBytes' = _fromBytesPlutusData (eh "PlutusData")

instance FromBytes TransactionUnspentOutput where
  fromBytes' = _fromBytesTransactionUnspentOutput
    (eh "TransactionUnspentOutput")

instance FromBytes TransactionWitnessSet where
  fromBytes' = _fromBytesTransactionWitnessSet (eh "TransactionWitnessSet")

instance FromBytes NativeScript where
  fromBytes' = _fromBytesNativeScript (eh "NativeScript")

instance FromBytes Mint where
  fromBytes' = _fromBytesMint (eh "Mint")

instance FromBytes VRFKeyHash where
  fromBytes' = _fromBytesVRFKeyHash (eh "VRFKeyHash")

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
eh :: forall r. String -> ErrorFfiHelper (FromBytesError + r)
eh = errorHelper <<< inj _fromBytesError

---- Foreign imports

foreign import _fromBytesDataHash
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r DataHash

foreign import _fromBytesTransactionHash
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r TransactionHash

foreign import _fromBytesPlutusData
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r PlutusData

foreign import _fromBytesTransaction
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r Transaction

foreign import _fromBytesTransactionUnspentOutput
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r TransactionUnspentOutput

foreign import _fromBytesTransactionWitnessSet
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r TransactionWitnessSet

foreign import _fromBytesNativeScript
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r NativeScript

foreign import _fromBytesMint
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r Mint

foreign import _fromBytesVRFKeyHash
  :: forall r. ErrorFfiHelper r -> ByteArray -> E r VRFKeyHash
