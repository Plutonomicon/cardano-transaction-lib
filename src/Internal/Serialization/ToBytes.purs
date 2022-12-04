module Ctl.Internal.Serialization.ToBytes
--  ( class ToBytes
  ( toBytes
--  , toBytes'
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

import Untagged.Union (type (|+|))

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import _toBytes
  :: ( Transaction
         |+| TransactionBody
         |+| TransactionOutput
         |+| TransactionUnspentOutput
         |+| TransactionHash
         |+| DataHash
         |+| PlutusData
         |+| TransactionWitnessSet
         |+| NativeScript
         |+| ScriptDataHash
         |+| Redeemers
         |+| GenesisHash
         |+| GenesisDelegateHash
         |+| AuxiliaryDataHash
         |+| Address
         |+| Value
         |+| Ed25519Signature
         |+| VRFKeyHash
     -- Add more as needed.
     )
  -> ByteArray
-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes

-- foreign import _toBytes :: forall (a :: Type). a -> ByteArray

-- class ToBytes a where
--   toBytes' :: a -> ByteArray

-- instance ToBytes Address where
--   toBytes' = _toBytes

-- instance ToBytes AuxiliaryDataHash where
--   toBytes' = _toBytes

-- instance ToBytes DataHash where
--   toBytes' = _toBytes

-- instance ToBytes Ed25519KeyHash where
--   toBytes' = _toBytes

-- instance ToBytes Ed25519Signature where
--   toBytes' = _toBytes

-- instance ToBytes GenesisDelegateHash where
--   toBytes' = _toBytes

-- instance ToBytes GenesisHash where
--   toBytes' = _toBytes

-- instance ToBytes NativeScript where
--   toBytes' = _toBytes

-- instance ToBytes PlutusData where
--   toBytes' = _toBytes

-- instance ToBytes PoolMetadataHash where
--   toBytes' = _toBytes

-- instance ToBytes Redeemers where
--   toBytes' = _toBytes

-- instance ToBytes ScriptDataHash where
--   toBytes' = _toBytes

-- instance ToBytes ScriptHash where
--   toBytes' = _toBytes

-- instance ToBytes Transaction where
--   toBytes' = _toBytes

-- instance ToBytes TransactionBody where
--   toBytes' = _toBytes

-- instance ToBytes TransactionHash where
--   toBytes' = _toBytes

-- instance ToBytes TransactionOutput where
--   toBytes' = _toBytes

-- instance ToBytes TransactionUnspentOutput where
--   toBytes' = _toBytes

-- instance ToBytes TransactionWitnessSet where
--   toBytes' = _toBytes

-- instance ToBytes Value where
--   toBytes' = _toBytes

-- instance ToBytes VRFKeyHash where
--   toBytes' = _toBytes

toBytes :: forall (a :: Type). ToBytes a => a -> CborBytes
toBytes = CborBytes <<< toBytes'
