module Ctl.Internal.Serialization.ToBytes (toBytes) where

import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Types
  ( AuxiliaryDataHash
  , DataHash
  , Ed25519Signature
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
import Ctl.Internal.Types.ByteArray (ByteArray)
import Untagged.Union (type (|+|))

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import toBytes
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
     -- Add more as needed.
     )
  -> ByteArray
