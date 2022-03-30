module Seabug.MintingPolicy
  ( mintingPolicy
  ) where

import Contract.Prelude
import Contract.Scripts (MintingPolicy)
import Data.Argonaut (Json, JsonDecodeError)
import Seabug.Helpers (jsonReader)

mintingPolicy :: Either JsonDecodeError MintingPolicy
mintingPolicy = jsonReader "mintingPolicy" _mintingPolicy

foreign import _mintingPolicy :: Json
