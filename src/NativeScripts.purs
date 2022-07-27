module NativeScripts where

import Prelude

import Cardano.Types.Transaction (NativeScript)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Serialization.Hash (ScriptHash)
import Serialization.Hash as Hashing
import Serialization.NativeScript (convertNativeScript)

newtype NativeScriptHash = NativeScriptHash ScriptHash

derive instance Newtype NativeScriptHash _
derive newtype instance Eq NativeScriptHash
derive newtype instance Ord NativeScriptHash

instance Show NativeScriptHash where
  show (NativeScriptHash sh) = "(NativeScriptHash " <> show sh <> ")"

nativeScriptHash :: NativeScript -> Maybe NativeScriptHash
nativeScriptHash ns = wrap <<< Hashing.nativeScriptHash <$> convertNativeScript
  ns
