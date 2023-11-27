module Ctl.Internal.Hashing
  ( blake2b224Hash
  , blake2b224HashHex
  , blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , hashPlutusData
  , md5HashHex
  , plutusScriptHash
  , scriptRefHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  , transactionHash
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
  ( DataHash
  , PlutusData
  , PlutusScript
  , Transaction
  ) as Serialization
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Scripts (PlutusScript)
import Ctl.Internal.Types.Transaction (DataHash, TransactionHash)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Node.Buffer (fromString, toString) as Buffer
import Node.Crypto.Hash (createHash, digest, update) as Hash
import Node.Encoding (Encoding(Hex, UTF8))

foreign import blake2b224Hash :: ByteArray -> ByteArray

foreign import blake2b224HashHex :: ByteArray -> String

foreign import blake2b256Hash :: ByteArray -> ByteArray

foreign import blake2b256HashHex :: ByteArray -> String

foreign import hashPlutusData
  :: Serialization.PlutusData -> Serialization.DataHash

foreign import hashPlutusScript :: Serialization.PlutusScript -> ScriptHash

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

md5HashHex :: String -> Effect String
md5HashHex contents = do
  buf <- Buffer.fromString contents UTF8
  digest <- Hash.createHash "md5" >>= Hash.update buf >>= Hash.digest
  Buffer.toString Hex digest

datumHash :: Datum -> DataHash
datumHash =
  wrap <<< unwrap <<< toBytes <<< hashPlutusData
    <<< convertPlutusData
    <<< unwrap

-- | Calculates the hash of the transaction by applying `blake2b256Hash` to
-- | the cbor-encoded transaction body.
transactionHash :: Serialization.Transaction -> TransactionHash
transactionHash =
  wrap <<< blake2b256Hash <<< unwrap <<< toBytes <<< _txBody

plutusScriptHash :: PlutusScript -> ScriptHash
plutusScriptHash = hashPlutusScript <<< convertPlutusScript

scriptRefHash :: ScriptRef -> ScriptHash
scriptRefHash (PlutusScriptRef plutusScript) = plutusScriptHash plutusScript
scriptRefHash (NativeScriptRef nativeScript) =
  nativeScriptHash $ convertNativeScript nativeScript
