module CTL.Internal.Deserialization.NativeScript
  ( convertNativeScript
  ) where

import Prelude

import CTL.Internal.Cardano.Types.NativeScript as T
import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import CTL.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import CTL.Internal.Serialization.Hash (Ed25519KeyHash)
import CTL.Internal.Serialization.Address (Slot(Slot))
import CTL.Internal.Serialization.Types
  ( NativeScript
  , ScriptAll
  , ScriptAny
  , ScriptNOfK
  , ScriptPubkey
  , TimelockExpiry
  , TimelockStart
  )
import CTL.Internal.Types.BigNum (BigNum)

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
  T.ScriptPubkey <<< scriptPubkey_addr_keyhash <$>
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
convertTimelockStart =
  map (T.TimelockStart <<< Slot <<< timelockStart_slot)
    <<< getTimelockStart maybeFfiHelper

convertTimelockExpiry :: NativeScript -> Maybe T.NativeScript
convertTimelockExpiry = do
  map (T.TimelockExpiry <<< Slot <<< timelockExpiry_slot)
    <<< getTimelockExpiry maybeFfiHelper

foreign import getScriptPubkey
  :: MaybeFfiHelper -> NativeScript -> Maybe ScriptPubkey

foreign import getScriptAll :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAll
foreign import getScriptAny :: MaybeFfiHelper -> NativeScript -> Maybe ScriptAny
foreign import getScriptNOfK
  :: MaybeFfiHelper -> NativeScript -> Maybe ScriptNOfK

foreign import getTimelockStart
  :: MaybeFfiHelper -> NativeScript -> Maybe TimelockStart

foreign import getTimelockExpiry
  :: MaybeFfiHelper -> NativeScript -> Maybe TimelockExpiry

foreign import scriptPubkey_addr_keyhash :: ScriptPubkey -> Ed25519KeyHash
foreign import scriptAllScripts
  :: ContainerHelper -> ScriptAll -> Array NativeScript

foreign import scriptAnyScripts
  :: ContainerHelper -> ScriptAny -> Array NativeScript

foreign import scriptNOfKScripts
  :: ContainerHelper -> ScriptNOfK -> Array NativeScript

foreign import scriptNOfK_n :: ScriptNOfK -> Int
foreign import timelockStart_slot :: TimelockStart -> BigNum
foreign import timelockExpiry_slot :: TimelockExpiry -> BigNum
