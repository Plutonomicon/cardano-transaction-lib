module Ctl.Internal.Serialization.ToBytes (toBytes) where

-- import Prelude

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
-- import Untagged.Castable (class Castable, cast)
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
     -- Add more as needed.
     )
  -> CborBytes
