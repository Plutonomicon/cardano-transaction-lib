module Seabug.MintingPolicy
  ( mintingPolicy
  ) where

import Contract.Prelude
import Contract.Scripts (MintingPolicy)
import Data.Argonaut (Json, JsonDecodeError)
import Seabug.Helpers (jsonReader)

-- This is read in locally as a typed validator.
-- Recall, Plutus typed validators map `Any` to `PlutusData` using associated
-- type families. We are restricted to functional dependencies in Purescript,
-- so are required to type with the output, namely, `PlutusData`.
mintingPolicy :: Either JsonDecodeError MintingPolicy
mintingPolicy = jsonReader "mintingPolicy" _mintingPolicy

foreign import _mintingPolicy :: Json
