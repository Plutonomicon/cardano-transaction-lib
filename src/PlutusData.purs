module PlutusData
  ( plutusDataBytes
  ) where

import Prelude

import Data.Maybe (Maybe)
import Deserialization.WitnessSet (convertPlutusData) as WS
import Serialization.PlutusData (convertPlutusData) as PD
import Types.PlutusData (PlutusData) as PD
import Types.Transaction (PlutusData) as T

-- | A module for Plutus Data related helpers to prevent cyclic dependencies

-- | Converts a built in (recursive) `PlutusData` to a serialised `ByteArray`
-- | `PlutusData` for CSL (via the foreign `PlutusData`).
plutusDataBytes :: PD.PlutusData -> Maybe T.PlutusData
plutusDataBytes = map WS.convertPlutusData <<< PD.convertPlutusData