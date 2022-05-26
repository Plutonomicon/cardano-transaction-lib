module Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , plutusScriptHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Show.Generic (genericShow)
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Serialization.PlutusData (convertPlutusData)
import Serialization.Types (PlutusData) as Serialization
import Types.ByteArray (ByteArray)
import Types.Datum (Datum)
import Types.Scripts (PlutusScript)
import Types.Transaction (DataHash)

foreign import blake2b256Hash :: ByteArray -> ByteArray

foreign import blake2b256HashHex :: ByteArray -> String

foreign import hashPlutusData :: Serialization.PlutusData -> ByteArray

foreign import hashPlutusScript :: PlutusScript -> ByteArray

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

datumHash :: Datum -> Maybe DataHash
datumHash =
  map (wrap <<< hashPlutusData) <<< convertPlutusData <<< unwrap

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash =
  scriptHashFromBytes <<< wrap <<< hashPlutusScript
