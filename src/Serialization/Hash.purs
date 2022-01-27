module Serialization.Hash (
  Ed25519KeyHash,
  ScriptHash
  ) where

import Control.Category (identity)
import Serialization.Csl (class ToCsl)


foreign import data Ed25519KeyHash :: Type
instance ToCsl Ed25519KeyHash Ed25519KeyHash where
  toCslRep = identity

foreign import data ScriptHash :: Type
instance ToCsl ScriptHash ScriptHash where
  toCslRep = identity
