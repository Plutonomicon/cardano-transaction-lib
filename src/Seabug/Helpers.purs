module Seabug.Helpers
  ( jsonReader
  ) where

import Contract.Prelude
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- | typed validator.
jsonReader
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> Aff (Either JsonDecodeError a)
jsonReader inputStr = do
  str <- readTextFile UTF8 inputStr
  pure $ either (Left <<< TypeMismatch) decodeJson (jsonParser str)