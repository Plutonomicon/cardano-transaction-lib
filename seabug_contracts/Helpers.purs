module Seabug.Helpers
  ( jsonReader
  ) where

import Contract.Prelude
import Data.Argonaut
  ( Json
  , class DecodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , getField
  )
import Foreign.Object (Object)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- | typed validator
jsonReader
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> Json
  -> Either JsonDecodeError a
jsonReader field = caseJsonObject (Left $ TypeMismatch "Expected Object")
  $ flip getField field
