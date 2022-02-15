module Deserialization.NativeScript where

import Prelude
import Undefined
import Serialization.Types
import Types.Transaction as T
import Data.Maybe
import Control.Alt
import FfiHelpers
import Deserialization.Address (convertEd25519KeyHash)

convertNativeScript :: NativeScript -> Maybe T.NativeScript
convertNativeScript ns =
  convertScriptPubKey ns
    <|> convertScriptAll ns
    <|> convertScriptAny ns
    <|> convertScriptNOfK ns
    <|> convertTimelockStart ns
    <|> convertTimelockExpiry ns

convertScriptPubKey :: NativeScript -> Maybe T.NativeScript
convertScriptPubKey ns = do
  T.ScriptPubkey <<< convertEd25519KeyHash <<< scriptPubkey_addr_keyhash <$>
    getScriptPubkey maybeFfiHelper ns

convertScriptAll :: NativeScript -> Maybe T.NativeScript
convertScriptAll = undefined

convertScriptAny :: NativeScript -> Maybe T.NativeScript
convertScriptAny = undefined

convertScriptNOfK :: NativeScript -> Maybe T.NativeScript
convertScriptNOfK = undefined

convertTimelockStart :: NativeScript -> Maybe T.NativeScript
convertTimelockStart = undefined

convertTimelockExpiry :: NativeScript -> Maybe T.NativeScript
convertTimelockExpiry = undefined

foreign import getScriptPubkey :: MaybeFfiHelper -> NativeScript -> Maybe ScriptPubkey
foreign import getScriptAll :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAll
foreign import getScriptAny :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAny
foreign import getScriptNOfK :: MaybeFfiHelper -> NativeScript -> Maybe ScriptNOfK
foreign import getTimelockStart :: MaybeFfiHelper -> NativeScript -> Maybe TimelockStart
foreign import getTimelockExpiry :: MaybeFfiHelper -> NativeScript -> Maybe TimelockExpiry

foreign import scriptPubkey_addr_keyhash :: ScriptPubkey -> Ed25519KeyHash
