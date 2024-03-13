module Ctl.Internal.Hashing
  ( blake2b224Hash
  , blake2b224HashHex
  , blake2b256Hash
  , blake2b256HashHex
  , plutusDataHash
  , md5HashHex
  , plutusScriptHash
  , scriptRefHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  , mintingPolicyHash
  ) where

import Prelude

import Cardano.Serialization.Lib
  ( hashPlutusData
  , nativeScript_hash
  , plutusScript_hash
  )
import Cardano.Types.DataHash (DataHash)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Types.MintingPolicy
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  )
import Data.ByteArray (ByteArray)
import Data.Newtype (wrap)
import Effect (Effect)
import Node.Buffer (fromString, toString) as Buffer
import Node.Crypto.Hash (createHash, digest, update) as Hash
import Node.Encoding (Encoding(Hex, UTF8))

foreign import blake2b224Hash :: ByteArray -> ByteArray

foreign import blake2b224HashHex :: ByteArray -> String

foreign import blake2b256Hash :: ByteArray -> ByteArray

foreign import blake2b256HashHex :: ByteArray -> String

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

md5HashHex :: String -> Effect String
md5HashHex contents = do
  buf <- Buffer.fromString contents UTF8
  digest <- Hash.createHash "md5" >>= Hash.update buf >>= Hash.digest
  Buffer.toString Hex digest

plutusDataHash :: PlutusData -> DataHash
plutusDataHash =
  wrap <<< hashPlutusData <<< PlutusData.toCsl

-- | Converts a Plutus-style `MintingPolicy` to an `MintingPolicyHash`
mintingPolicyHash :: MintingPolicy -> ScriptHash
mintingPolicyHash = case _ of
  PlutusMintingPolicy script -> plutusScriptHash script
  NativeMintingPolicy nscript -> nativeScriptHash nscript

-- -- can't fail, because the length is correct
-- wrap $ unsafePartial $ fromJust $ fromBytes $ blake2b256Hash $ toBytes
--   $ _txBody
--   $ tx

plutusScriptHash :: PlutusScript -> ScriptHash
plutusScriptHash = wrap <<< plutusScript_hash <<< PlutusScript.toCsl

nativeScriptHash :: NativeScript -> ScriptHash
nativeScriptHash = wrap <<< nativeScript_hash <<< NativeScript.toCsl

scriptRefHash :: ScriptRef -> ScriptHash
scriptRefHash (PlutusScriptRef plutusScript) = plutusScriptHash plutusScript
scriptRefHash (NativeScriptRef nativeScript) = nativeScriptHash nativeScript
