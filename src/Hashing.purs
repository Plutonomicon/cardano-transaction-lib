module Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , plutusScriptHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  , transactionHash
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Deserialization.Transaction (_txBody)
import Serialization (toBytes)
import Serialization.Hash (ScriptHash)
import Serialization.PlutusData (convertPlutusData)
import Serialization.Types (PlutusData, Transaction) as Serialization
import Types.ByteArray (ByteArray)
import Types.Datum (Datum)
import Types.Scripts (PlutusScript)
import Types.Transaction (DataHash, TransactionHash)
import Untagged.Union (asOneOf)

foreign import blake2b256Hash :: ByteArray -> ByteArray

foreign import blake2b256HashHex :: ByteArray -> String

foreign import hashPlutusData :: Serialization.PlutusData -> ByteArray

foreign import plutusScriptHash :: PlutusScript -> ScriptHash

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

datumHash :: Datum -> Maybe DataHash
datumHash =
  map (wrap <<< hashPlutusData) <<< convertPlutusData <<< unwrap

-- | Calculates the hash of the transaction by applying `blake2b256Hash` to
-- | the cbor-encoded transaction body.
transactionHash :: Serialization.Transaction -> TransactionHash
transactionHash =
  wrap <<< blake2b256Hash <<< toBytes <<< asOneOf <<< _txBody
