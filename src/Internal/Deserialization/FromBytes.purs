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
import Ctl.Internal.Serialization.Address
  ( Address
  , ByronAddress
  , StakeCredential
  )
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
  fromBytes' = fromBytes'' "Address"

instance FromBytes AuxiliaryDataHash where
  fromBytes' = fromBytes'' "AuxiliaryDataHash"

instance FromBytes ByronAddress where
  fromBytes' = fromBytes'' "ByronAddress"

instance FromBytes DataHash where
  fromBytes' = fromBytes'' "DataHash"

instance FromBytes Ed25519KeyHash where
  fromBytes' = fromBytes'' "Ed25519KeyHash"

instance FromBytes Ed25519Signature where
  fromBytes' = fromBytes'' "Ed25519Signature"

instance FromBytes GenesisDelegateHash where
  fromBytes' = fromBytes'' "GenesisDelegateHash"

instance FromBytes GenesisHash where
  fromBytes' = fromBytes'' "GenesisHash"

instance FromBytes Mint where
  fromBytes' = fromBytes'' "Mint"

instance FromBytes NativeScript where
  fromBytes' = fromBytes'' "NativeScript"

instance FromBytes PlutusData where
  fromBytes' = fromBytes'' "PlutusData"

instance FromBytes PoolMetadataHash where
  fromBytes' = fromBytes'' "PoolMetadataHash"

instance FromBytes PublicKey where
  fromBytes' = fromBytes'' "PublicKey"

instance FromBytes Redeemers where
  fromBytes' = fromBytes'' "Redeemers"

instance FromBytes ScriptDataHash where
  fromBytes' = fromBytes'' "ScriptDataHash"

instance FromBytes ScriptHash where
  fromBytes' = fromBytes'' "ScriptHash"

instance FromBytes StakeCredential where
  fromBytes' = fromBytes'' "StakeCredential"

instance FromBytes Transaction where
  fromBytes' = fromBytes'' "Transaction"

instance FromBytes TransactionBody where
  fromBytes' = fromBytes'' "TransactionBody"

instance FromBytes TransactionHash where
  fromBytes' = fromBytes'' "TransactionHash"

instance FromBytes TransactionOutput where
  fromBytes' = fromBytes'' "TransactionOutput"

instance FromBytes TransactionUnspentOutput where
  fromBytes' = fromBytes'' "TransactionUnspentOutput"

instance FromBytes TransactionWitnessSet where
  fromBytes' = fromBytes'' "TransactionWitnessSet"

instance FromBytes Value where
  fromBytes' = fromBytes'' "Value"

instance FromBytes VRFKeyHash where
  fromBytes' = fromBytes'' "VRFKeyHash"

-- for backward compatibility until `Maybe` is abandoned. Then to be renamed.
fromBytes :: forall (a :: Type). FromBytes a => CborBytes -> Maybe a
fromBytes = unwrap >>> fromBytes' >>> hush

fromBytesEffect :: forall (a :: Type). FromBytes a => CborBytes -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

fromBytes''
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> ByteArray
  -> E (FromBytesError + r) a
fromBytes'' = flip _fromBytes fromBytesErrorHelper

---- Foreign imports

foreign import _fromBytes
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> ErrorFfiHelper r
  -> ByteArray
  -> E r a
