module CTL.Internal.Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , plutusScriptHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  , transactionHash
  , scriptRefHash
  ) where

import Prelude

import CTL.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import CTL.Internal.Deserialization.Transaction (_txBody)
import CTL.Internal.Serialization (toBytes)
import CTL.Internal.Serialization.Hash (ScriptHash, nativeScriptHash)
import CTL.Internal.Serialization.NativeScript (convertNativeScript)
import CTL.Internal.Serialization.PlutusData (convertPlutusData)
import CTL.Internal.Serialization.PlutusScript (convertPlutusScript)
import CTL.Internal.Serialization.Types
  ( PlutusData
  , PlutusScript
  , Transaction
  ) as Serialization
import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.Datum (Datum)
import CTL.Internal.Types.Scripts (PlutusScript)
import CTL.Internal.Types.Transaction (DataHash, TransactionHash)
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap, wrap)
import Untagged.Union (asOneOf)

foreign import blake2b256Hash :: ByteArray -> ByteArray

foreign import blake2b256HashHex :: ByteArray -> String

foreign import hashPlutusData :: Serialization.PlutusData -> ByteArray

foreign import hashPlutusScript :: Serialization.PlutusScript -> ScriptHash

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

plutusScriptHash :: PlutusScript -> ScriptHash
plutusScriptHash = hashPlutusScript <<< convertPlutusScript

scriptRefHash :: ScriptRef -> Maybe ScriptHash
scriptRefHash (PlutusScriptRef plutusScript) =
  Just (plutusScriptHash plutusScript)
scriptRefHash (NativeScriptRef nativeScript) =
  nativeScriptHash <$> convertNativeScript nativeScript
