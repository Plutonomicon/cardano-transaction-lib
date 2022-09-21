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

import Data.Either (Either(Left), hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Exception (throw)
import Error (E)
import FfiHelpers (ErrorFfiHelper, errorHelper)
import Serialization.Types
  ( DataHash
  , Ed25519Signature
  , Mint
  , NativeScript
  , PlutusData
  , PublicKey
  , Transaction
  , TransactionHash
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , VRFKeyHash
  , Value
  )
import Type.Prelude (Proxy(Proxy))
import Type.Row (type (+))
import Types.ByteArray (ByteArray)

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes' :: forall (r :: Row Type). ByteArray -> E (FromBytesError + r) a

instance FromBytes DataHash where
  fromBytes' = _fromBytesDataHash eh

instance FromBytes Transaction where
  fromBytes' = _fromBytesTransaction eh

instance FromBytes TransactionHash where
  fromBytes' = _fromBytesTransactionHash eh

instance FromBytes PlutusData where
  fromBytes' = _fromBytesPlutusData eh

instance FromBytes TransactionUnspentOutput where
  fromBytes' = _fromBytesTransactionUnspentOutput eh

instance FromBytes TransactionWitnessSet where
  fromBytes' = _fromBytesTransactionWitnessSet eh

instance FromBytes NativeScript where
  fromBytes' = _fromBytesNativeScript eh

instance FromBytes Mint where
  fromBytes' = _fromBytesMint eh

instance FromBytes VRFKeyHash where
  fromBytes' = _fromBytesVRFKeyHash eh

instance FromBytes Value where
  fromBytes' = _fromBytesValue eh

instance FromBytes PublicKey where
  fromBytes' = _fromBytesPublicKey eh

instance FromBytes Ed25519Signature where
  fromBytes' = _fromBytesEd25519Signature eh

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

foreign import _fromBytesDataHash
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r DataHash

foreign import _fromBytesTransactionHash
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> ByteArray
  -> E r TransactionHash

foreign import _fromBytesPlutusData
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r PlutusData

foreign import _fromBytesTransaction
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r Transaction

foreign import _fromBytesTransactionUnspentOutput
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> ByteArray
  -> E r TransactionUnspentOutput

foreign import _fromBytesTransactionWitnessSet
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> ByteArray
  -> E r TransactionWitnessSet

foreign import _fromBytesNativeScript
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r NativeScript

foreign import _fromBytesMint
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r Mint

foreign import _fromBytesVRFKeyHash
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r VRFKeyHash

foreign import _fromBytesValue
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r Value

foreign import _fromBytesPublicKey
  :: forall (r :: Row Type). ErrorFfiHelper r -> ByteArray -> E r PublicKey

foreign import _fromBytesEd25519Signature
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> ByteArray
  -> E r Ed25519Signature

