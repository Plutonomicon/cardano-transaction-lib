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

import Control.Promise (Promise)
import Control.Promise (toAffE) as Promise
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Deserialization.Transaction (_txBody)
import Effect (Effect)
import Effect.Aff (Aff)
import Serialization (toBytes)
import Serialization.Hash (ScriptHash)
import Serialization.PlutusData (convertPlutusData)
import Serialization.Types (PlutusData, Transaction) as Serialization
import Types.ByteArray (ByteArray)
import Types.Datum (Datum)
import Types.Scripts (PlutusScript)
import Types.Transaction (DataHash, TransactionHash)
import Untagged.Union (asOneOf)

foreign import _blake2bReady :: Effect (Promise Unit)

foreign import unsafeBlake2b256Hash :: ByteArray -> ByteArray

foreign import unsafeBlake2b256HashHex :: ByteArray -> String

foreign import hashPlutusData :: Serialization.PlutusData -> ByteArray

foreign import plutusScriptHash :: PlutusScript -> ScriptHash

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

blake2bReady :: Aff Unit
blake2bReady = Promise.toAffE _blake2bReady

blake2b256Hash :: ByteArray -> Aff ByteArray
blake2b256Hash bytes =
  blake2bReady >>= \_ -> pure (unsafeBlake2b256Hash bytes)

blake2b256HashHex :: ByteArray -> Aff String
blake2b256HashHex bytes =
  blake2bReady >>= \_ -> pure (unsafeBlake2b256HashHex bytes)

datumHash :: Datum -> Maybe DataHash
datumHash =
  map (wrap <<< hashPlutusData) <<< convertPlutusData <<< unwrap

-- | Calculates the hash of the transaction by applying `blake2b256Hash` to
-- | the cbor-encoded transaction body.
transactionHash :: Serialization.Transaction -> Aff TransactionHash
transactionHash =
  map wrap <<< blake2b256Hash <<< toBytes <<< asOneOf <<< _txBody
