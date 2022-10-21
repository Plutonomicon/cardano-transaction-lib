module Ctl.Internal.Hashing
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

import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Deserialization.Transaction (_txBody)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.Hash (ScriptHash, nativeScriptHash)
import Ctl.Internal.Serialization.NativeScript (convertNativeScript)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript)
import Ctl.Internal.Serialization.Types
  ( PlutusData
  , PlutusScript
  , Transaction
  ) as Serialization
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Scripts (PlutusScript)
import Ctl.Internal.Types.Transaction (DataHash, TransactionHash)
import Data.Maybe (Maybe)
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

scriptRefHash :: ScriptRef -> ScriptHash
scriptRefHash (PlutusScriptRef plutusScript) = plutusScriptHash plutusScript
scriptRefHash (NativeScriptRef nativeScript) =
  nativeScriptHash $ convertNativeScript nativeScript
