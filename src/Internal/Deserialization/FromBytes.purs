module Ctl.Internal.Deserialization.FromBytes
  ( class FromBytes
  , fromBytes'
  , fromBytes
  , fromBytesEffect
  ) where

import Prelude

import Ctl.Internal.Deserialization.Error (FromBytesError, fromBytesErrorHelper)
import Ctl.Internal.Error (E)
import Ctl.Internal.FfiHelpers (ErrorFfiHelper)
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
-- import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Exception (throw)
import Type.Row (type (+))

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes' :: forall (r :: Row Type). CborBytes -> E (FromBytesError + r) a

instance FromBytes DataHash where
  fromBytes' = _fromBytes "DataHash" fromBytesErrorHelper

instance FromBytes Transaction where
  fromBytes' = _fromBytes "Transaction" fromBytesErrorHelper

instance FromBytes TransactionHash where
  fromBytes' = _fromBytes "TransactionHash" fromBytesErrorHelper

instance FromBytes PlutusData where
  fromBytes' = _fromBytes "PlutusData" fromBytesErrorHelper

instance FromBytes TransactionUnspentOutput where
  fromBytes' = _fromBytes "TransactionUnspentOutput" fromBytesErrorHelper

instance FromBytes TransactionWitnessSet where
  fromBytes' = _fromBytes "TransactionWitnessSet" fromBytesErrorHelper

instance FromBytes NativeScript where
  fromBytes' = _fromBytes "NativeScript" fromBytesErrorHelper

instance FromBytes Mint where
  fromBytes' = _fromBytes "Mint" fromBytesErrorHelper

instance FromBytes VRFKeyHash where
  fromBytes' = _fromBytes "VRFKeyHash" fromBytesErrorHelper

instance FromBytes Value where
  fromBytes' = _fromBytes "Value" fromBytesErrorHelper

-- for backward compatibility until `Maybe` is abandoned. Then to be renamed.
fromBytes :: forall (a :: Type). FromBytes a => CborBytes -> Maybe a
fromBytes = fromBytes' >>> hush

fromBytesEffect :: forall (a :: Type). FromBytes a => CborBytes -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

---- Foreign imports

foreign import _fromBytes
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> ErrorFfiHelper r
  -> CborBytes
  -> E r a
