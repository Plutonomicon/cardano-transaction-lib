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
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ScriptHash, VRFKeyHash)
import Ctl.Internal.Serialization.Types
  ( AuxiliaryDataHash
  , DataHash
  , Ed25519Signature
  , GenesisDelegateHash
  , GenesisHash
  , Mint
  , NativeScript
  , PlutusData
  , PoolMetadataHash
  , PublicKey
  , Redeemers
  , ScriptDataHash
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionOutput
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , Value
  )
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Exception (throw)
import Type.Row (type (+))

-- | Calls `from_bytes` method for the appropriate type
class FromBytes a where
  fromBytes' :: forall (r :: Row Type). ByteArray -> E (FromBytesError + r) a

instance FromBytes Address where
  fromBytes' = _fromBytes "Address" fromBytesErrorHelper

instance FromBytes AuxiliaryDataHash where
  fromBytes' = _fromBytes "AuxiliaryDataHash" fromBytesErrorHelper

instance FromBytes DataHash where
  fromBytes' = _fromBytes "DataHash" fromBytesErrorHelper

instance FromBytes Ed25519KeyHash where
  fromBytes' = _fromBytes "Ed25519KeyHash" fromBytesErrorHelper

instance FromBytes Ed25519Signature where
  fromBytes' = _fromBytes "Ed25519Signature" fromBytesErrorHelper

instance FromBytes GenesisDelegateHash where
  fromBytes' = _fromBytes "GenesisDelegateHash" fromBytesErrorHelper

instance FromBytes GenesisHash where
  fromBytes' = _fromBytes "GenesisHash" fromBytesErrorHelper

instance FromBytes Mint where
  fromBytes' = _fromBytes "Mint" fromBytesErrorHelper

instance FromBytes NativeScript where
  fromBytes' = _fromBytes "NativeScript" fromBytesErrorHelper

instance FromBytes PlutusData where
  fromBytes' = _fromBytes "PlutusData" fromBytesErrorHelper

instance FromBytes PoolMetadataHash where
  fromBytes' = _fromBytes "PoolMetadataHash" fromBytesErrorHelper

instance FromBytes Redeemers where
  fromBytes' = _fromBytes "Redeemers" fromBytesErrorHelper

instance FromBytes ScriptDataHash where
  fromBytes' = _fromBytes "ScriptDataHash" fromBytesErrorHelper

instance FromBytes ScriptHash where
  fromBytes' = _fromBytes "ScriptHash" fromBytesErrorHelper

instance FromBytes Transaction where
  fromBytes' = _fromBytes "Transaction" fromBytesErrorHelper

instance FromBytes TransactionBody where
  fromBytes' = _fromBytes "TransactionBody" fromBytesErrorHelper

instance FromBytes TransactionHash where
  fromBytes' = _fromBytes "TransactionHash" fromBytesErrorHelper

instance FromBytes TransactionOutput where
  fromBytes' = _fromBytes "TransactionOutput" fromBytesErrorHelper

instance FromBytes TransactionUnspentOutput where
  fromBytes' = _fromBytes "TransactionUnspentOutput" fromBytesErrorHelper

instance FromBytes TransactionWitnessSet where
  fromBytes' = _fromBytes "TransactionWitnessSet" fromBytesErrorHelper

instance FromBytes Value where
  fromBytes' = _fromBytes "Value" fromBytesErrorHelper

instance FromBytes VRFKeyHash where
  fromBytes' = _fromBytes "VRFKeyHash" fromBytesErrorHelper

-- for backward compatibility until `Maybe` is abandoned. Then to be renamed.
fromBytes :: forall (a :: Type). FromBytes a => CborBytes -> Maybe a
fromBytes = unwrap >>> fromBytes' >>> hush

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
  -> ByteArray
  -> E r a
