module Ctl.Internal.Serialization.ToBytes
  ( toBytes
  ) where

import Prelude

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
  , Redeemers
  , ScriptDataHash
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionOutput
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , Value
  , Vkeywitness
  , Vkeywitnesses
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Untagged.Castable (class Castable)
import Untagged.Union (type (|+|))

type SerializableData = Address
  |+| AuxiliaryDataHash
  |+| ByronAddress
  |+| DataHash
  |+| Ed25519KeyHash
  |+| Ed25519Signature
  |+| GenesisDelegateHash
  |+| GenesisHash
  |+| Mint
  |+| NativeScript
  |+| PlutusData
  |+| PoolMetadataHash
  |+| Redeemers
  |+| ScriptDataHash
  |+| ScriptHash
  |+| StakeCredential
  |+| Transaction
  |+| TransactionBody
  |+| TransactionHash
  |+| TransactionOutput
  |+| TransactionUnspentOutput
  |+| TransactionWitnessSet
  |+| Value
  |+| VRFKeyHash
  |+| Vkeywitness
  |+| Vkeywitnesses
  |+| BigNum

-- Add more as needed

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import _toBytes
  :: forall a. a -> ByteArray

toBytes
  :: forall a
   . Castable a SerializableData
  => a
  -> CborBytes
toBytes = CborBytes <<< _toBytes
