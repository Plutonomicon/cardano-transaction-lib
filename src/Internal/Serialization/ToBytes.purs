module Ctl.Internal.Serialization.ToBytes
  ( class ToBytes
  , toBytes
  , toBytes'
  ) where

import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Types
  ( AuxiliaryDataHash
  , DataHash
  , GenesisDelegateHash
  , GenesisHash
  , NativeScript
  , PlutusData
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
import Ctl.Internal.Types.CborBytes (CborBytes)

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import _toBytes :: forall (a :: Type). a -> CborBytes

class ToBytes a where
  toBytes' :: a -> CborBytes

instance ToBytes Address where
  toBytes' = _toBytes

instance ToBytes AuxiliaryDataHash where
  toBytes' = _toBytes

instance ToBytes DataHash where
  toBytes' = _toBytes

instance ToBytes GenesisDelegateHash where
  toBytes' = _toBytes

instance ToBytes GenesisHash where
  toBytes' = _toBytes

instance ToBytes NativeScript where
  toBytes' = _toBytes

instance ToBytes PlutusData where
  toBytes' = _toBytes

instance ToBytes Redeemers where
  toBytes' = _toBytes

instance ToBytes ScriptDataHash where
  toBytes' = _toBytes

instance ToBytes Transaction where
  toBytes' = _toBytes

instance ToBytes TransactionBody where
  toBytes' = _toBytes

instance ToBytes TransactionHash where
  toBytes' = _toBytes

instance ToBytes TransactionOutput where
  toBytes' = _toBytes

instance ToBytes TransactionUnspentOutput where
  toBytes' = _toBytes

instance ToBytes TransactionWitnessSet where
  toBytes' = _toBytes

instance ToBytes Value where
  toBytes' = _toBytes

toBytes :: forall (a :: Type). ToBytes a => a -> CborBytes
toBytes = toBytes'
