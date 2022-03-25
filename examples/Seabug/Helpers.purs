module Seabug.Helpers
  ( jsonReader
  ) where

import Contract.Prelude
import Data.Argonaut.Core (caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- | typed validator.
jsonReader
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> String
  -> Aff (Either JsonDecodeError a)
jsonReader path field = do
  str <- readTextFile UTF8 path
  pure $ either (Left <<< TypeMismatch) getInput (jsonParser str)
  where
  getInput = caseJsonObject
    (Left $ TypeMismatch "Expected Object")
    (flip getField field)
