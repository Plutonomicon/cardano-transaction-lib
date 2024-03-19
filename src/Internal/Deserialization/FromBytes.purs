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
  , GeneralTransactionMetadata
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
  , TransactionMetadatum
  , TransactionOutput
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , Value
  , Vkeywitness
  , Vkeywitnesses
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
  fromBytes' = fromBytesImpl "Address"

instance FromBytes AuxiliaryDataHash where
  fromBytes' = fromBytesImpl "AuxiliaryDataHash"

instance FromBytes ByronAddress where
  fromBytes' = fromBytesImpl "ByronAddress"

instance FromBytes DataHash where
  fromBytes' = fromBytesImpl "DataHash"

instance FromBytes Ed25519KeyHash where
  fromBytes' = fromBytesImpl "Ed25519KeyHash"

instance FromBytes Ed25519Signature where
  fromBytes' = fromBytesImpl "Ed25519Signature"

instance FromBytes GeneralTransactionMetadata where
  fromBytes' = fromBytesImpl "GeneralTransactionMetadata"

instance FromBytes TransactionMetadatum where
  fromBytes' = fromBytesImpl "TransactionMetadatum"

instance FromBytes GenesisDelegateHash where
  fromBytes' = fromBytesImpl "GenesisDelegateHash"

instance FromBytes GenesisHash where
  fromBytes' = fromBytesImpl "GenesisHash"

instance FromBytes Mint where
  fromBytes' = fromBytesImpl "Mint"

instance FromBytes NativeScript where
  fromBytes' = fromBytesImpl "NativeScript"

instance FromBytes PlutusData where
  fromBytes' = fromBytesImpl "PlutusData"

instance FromBytes PoolMetadataHash where
  fromBytes' = fromBytesImpl "PoolMetadataHash"

instance FromBytes PublicKey where
  fromBytes' = fromBytesImpl "PublicKey"

instance FromBytes Redeemers where
  fromBytes' = fromBytesImpl "Redeemers"

instance FromBytes ScriptDataHash where
  fromBytes' = fromBytesImpl "ScriptDataHash"

instance FromBytes ScriptHash where
  fromBytes' = fromBytesImpl "ScriptHash"

instance FromBytes StakeCredential where
  fromBytes' = fromBytesImpl "Credential"

instance FromBytes Transaction where
  fromBytes' = fromBytesImpl "Transaction"

instance FromBytes TransactionBody where
  fromBytes' = fromBytesImpl "TransactionBody"

instance FromBytes TransactionHash where
  fromBytes' = fromBytesImpl "TransactionHash"

instance FromBytes TransactionOutput where
  fromBytes' = fromBytesImpl "TransactionOutput"

instance FromBytes TransactionUnspentOutput where
  fromBytes' = fromBytesImpl "TransactionUnspentOutput"

instance FromBytes TransactionWitnessSet where
  fromBytes' = fromBytesImpl "TransactionWitnessSet"

instance FromBytes Value where
  fromBytes' = fromBytesImpl "Value"

instance FromBytes VRFKeyHash where
  fromBytes' = fromBytesImpl "VRFKeyHash"

instance FromBytes Vkeywitness where
  fromBytes' = fromBytesImpl "Vkeywitness"

instance FromBytes Vkeywitnesses where
  fromBytes' = fromBytesImpl "Vkeywitnesses"

-- for backward compatibility until `Maybe` is abandoned. Then to be renamed.
fromBytes :: forall (a :: Type). FromBytes a => CborBytes -> Maybe a
fromBytes = unwrap >>> fromBytes' >>> hush

fromBytesEffect :: forall (a :: Type). FromBytes a => CborBytes -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a

fromBytesImpl
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> ByteArray
  -> E (FromBytesError + r) a
fromBytesImpl = _fromBytes fromBytesErrorHelper

---- Foreign imports

foreign import _fromBytes
  :: forall (r :: Row Type) (a :: Type)
   . ErrorFfiHelper r
  -> String
  -> ByteArray
  -> E r a
