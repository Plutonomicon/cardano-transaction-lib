module Ctl.Internal.Serialization.ToBytes
  ( toBytes
  ) where

import Prelude

import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ScriptHash, VRFKeyHash)
import Ctl.Internal.Serialization.Types
  ( AuxiliaryDataHash
  , DataHash
  , Ed25519Signature
  , GenesisDelegateHash
  , GenesisHash
  , NativeScript
  , PlutusData
  , PoolMetadataHash
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
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Untagged.Castable (class Castable)
import Untagged.Union (type (|+|))

type SerializationData = Address
  |+| AuxiliaryDataHash
  |+| DataHash
  |+| Ed25519KeyHash
  |+| Ed25519Signature
  |+| GenesisDelegateHash
  |+| GenesisHash
  |+| NativeScript
  |+| PlutusData
  |+| PoolMetadataHash
  |+| Redeemers
  |+| ScriptDataHash
  |+| ScriptHash
  |+| Transaction
  |+| TransactionBody
  |+| TransactionHash
  |+| TransactionOutput
  |+| TransactionUnspentOutput
  |+| TransactionWitnessSet
  |+| Value
  |+| VRFKeyHash

-- and more as needed

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import _toBytes
  :: forall a. a -> ByteArray

toBytes
  :: forall a
   . Castable a SerializationData
  => a
  -> CborBytes
toBytes = CborBytes <<< _toBytes
