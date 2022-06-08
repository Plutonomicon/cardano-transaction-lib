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

import Control.Promise (Promise)
import Control.Promise (toAffE) as Promise
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Serialization.PlutusData (convertPlutusData)
import Serialization.Types (PlutusData) as Serialization
import Types.ByteArray (ByteArray)
import Types.Datum (Datum)
import Types.Scripts (PlutusScript)
import Types.Transaction (DataHash)

foreign import _blake2b256Hash :: ByteArray -> Effect (Promise ByteArray)

foreign import _blake2b256HashHex :: ByteArray -> Effect (Promise String)

foreign import hashPlutusData :: Serialization.PlutusData -> ByteArray

foreign import hashPlutusScript :: PlutusScript -> ByteArray

foreign import sha256Hash :: ByteArray -> ByteArray

foreign import sha256HashHex :: ByteArray -> String

foreign import sha3_256Hash :: ByteArray -> ByteArray

foreign import sha3_256HashHex :: ByteArray -> String

blake2b256Hash :: ByteArray -> Aff ByteArray
blake2b256Hash = Promise.toAffE <<< _blake2b256Hash

blake2b256HashHex :: ByteArray -> Aff String
blake2b256HashHex = Promise.toAffE <<< _blake2b256HashHex

datumHash :: Datum -> Maybe DataHash
datumHash =
  map (wrap <<< hashPlutusData) <<< convertPlutusData <<< unwrap

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash =
  scriptHashFromBytes <<< wrap <<< hashPlutusScript
