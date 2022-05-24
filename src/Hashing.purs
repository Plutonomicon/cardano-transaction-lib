module Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , hashDatum
  , hashPlutusScript
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

foreign import _hashPlutusScript :: PlutusScript -> ByteArray

hashDatum :: Datum -> Maybe DataHash
hashDatum =
  map (wrap <<< hashPlutusData) <<< convertPlutusData <<< unwrap

hashPlutusScript :: PlutusScript -> Maybe ScriptHash
hashPlutusScript =
  scriptHashFromBytes <<< wrap <<< _hashPlutusScript
