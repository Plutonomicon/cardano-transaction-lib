module Seabug.MarketPlace
  ( marketplaceValidator
  ) where

import Contract.Prelude
import Contract.PlutusData (PlutusData)
import Contract.Scripts (TypedValidator)
import Data.Argonaut (Json, JsonDecodeError)
import Seabug.Helpers (jsonReader)

-- This is read in locally as a typed validator.
-- Recall, Plutus typed validators map `Any` to `PlutusData` using associated
-- type families. We are restricted to functional dependencies in Purescript,
-- so are required to type with the output, namely, `PlutusData`.
marketplaceValidator :: Either JsonDecodeError (TypedValidator PlutusData)
marketplaceValidator = jsonReader "typedValidator" _marketplaceValidator

foreign import _marketplaceValidator :: Json
