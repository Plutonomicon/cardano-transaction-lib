module Ctl.Internal.NativeScripts
  ( NativeScriptHash(NativeScriptHash)
  , nativeScriptHash
  ) where

import Prelude

import Cardano.Data.Lite (nativeScript_hash)
import Cardano.Types (ScriptHash)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Data.Newtype (class Newtype, wrap)

newtype NativeScriptHash = NativeScriptHash ScriptHash

derive instance Newtype NativeScriptHash _
derive newtype instance Eq NativeScriptHash
derive newtype instance Ord NativeScriptHash

instance Show NativeScriptHash where
  show (NativeScriptHash sh) = "(NativeScriptHash " <> show sh <> ")"

nativeScriptHash :: NativeScript -> NativeScriptHash
nativeScriptHash = wrap <<< wrap <<< nativeScript_hash <<< NativeScript.toCdl

