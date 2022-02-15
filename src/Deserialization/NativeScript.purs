module Deserialization.NativeScript
  ( convertNativeScript
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.BigInt as BigInt
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

import Deserialization.Address (convertEd25519KeyHash)
import FfiHelpers (ContainerHelper, MaybeFfiHelper, containerHelper, maybeFfiHelper)
import Serialization.Types (Ed25519KeyHash, NativeScript, ScriptAll, ScriptAny, ScriptNOfK, ScriptPubkey, TimelockExpiry, TimelockStart)
import Types.Transaction as T

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
convertScriptAll ns = do
  scriptAll <- getScriptAll maybeFfiHelper ns
  T.ScriptAll <$> traverse convertNativeScript
    (scriptAllScripts containerHelper scriptAll)

convertScriptAny :: NativeScript -> Maybe T.NativeScript
convertScriptAny ns = do
  scriptAny <- getScriptAny maybeFfiHelper ns
  T.ScriptAny <$> traverse convertNativeScript
    (scriptAnyScripts containerHelper scriptAny)

convertScriptNOfK :: NativeScript -> Maybe T.NativeScript
convertScriptNOfK ns = do
  scriptNOfK <- getScriptNOfK maybeFfiHelper ns
  res <- traverse convertNativeScript
    (scriptNOfKScripts containerHelper scriptNOfK)
  pure $ T.ScriptNOfK (scriptNOfK_n scriptNOfK) res

convertTimelockStart :: NativeScript -> Maybe T.NativeScript
convertTimelockStart ns =
  T.TimelockStart <<< T.Slot <<< BigInt.fromInt <<< timelockStart_slot <$>
    getTimelockStart maybeFfiHelper ns

convertTimelockExpiry :: NativeScript -> Maybe T.NativeScript
convertTimelockExpiry ns = do
  T.TimelockExpiry <<< T.Slot <<< BigInt.fromInt <<< timelockExpiry_slot <$>
    getTimelockExpiry maybeFfiHelper ns

foreign import getScriptPubkey :: MaybeFfiHelper -> NativeScript -> Maybe ScriptPubkey
foreign import getScriptAll :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAll
foreign import getScriptAny :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAny
foreign import getScriptNOfK :: MaybeFfiHelper -> NativeScript -> Maybe ScriptNOfK
foreign import getTimelockStart :: MaybeFfiHelper -> NativeScript -> Maybe TimelockStart
foreign import getTimelockExpiry :: MaybeFfiHelper -> NativeScript -> Maybe TimelockExpiry

foreign import scriptPubkey_addr_keyhash :: ScriptPubkey -> Ed25519KeyHash
foreign import scriptAllScripts :: ContainerHelper -> ScriptAll -> Array NativeScript
foreign import scriptAnyScripts :: ContainerHelper -> ScriptAny -> Array NativeScript
foreign import scriptNOfKScripts :: ContainerHelper -> ScriptNOfK -> Array NativeScript
foreign import scriptNOfK_n :: ScriptNOfK -> Int
foreign import timelockStart_slot :: TimelockStart -> Int
foreign import timelockExpiry_slot :: TimelockExpiry -> Int
