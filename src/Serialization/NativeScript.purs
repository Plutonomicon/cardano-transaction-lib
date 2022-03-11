module Serialization.NativeScript
  ( convertNativeScript
  , convertNativeScripts
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)
import Data.UInt as UInt

import FfiHelpers (ContainerHelper, containerHelper)
import Serialization.Address (Slot(Slot)) as T
import Serialization.Hash (Ed25519KeyHash) as T
import Serialization.Types
  ( NativeScript
  , NativeScripts
  , ScriptAll
  , ScriptAny
  , ScriptNOfK
  , ScriptPubkey
  , TimelockExpiry
  , TimelockStart
  )
import Types.Transaction
  ( NativeScript(ScriptPubkey, ScriptAll, ScriptAny, ScriptNOfK, TimelockStart, TimelockExpiry)
  ) as T

convertNativeScripts :: Array T.NativeScript -> Maybe NativeScripts
convertNativeScripts = map packNativeScripts <<< traverse convertNativeScript

-- | Note: unbounded recursion here.
convertNativeScript :: T.NativeScript -> Maybe NativeScript
convertNativeScript = case _ of
  T.ScriptPubkey keyHash -> pure $ convertScriptPubkey keyHash
  T.ScriptAll nss -> convertScriptAll nss
  T.ScriptAny nss -> convertScriptAny nss
  T.ScriptNOfK n nss -> convertScriptNOfK n nss
  T.TimelockStart slot -> pure $ convertTimelockStart slot
  T.TimelockExpiry slot -> pure $ convertTimelockExpiry slot

convertScriptPubkey :: T.Ed25519KeyHash -> NativeScript
convertScriptPubkey hash = do
  nativeScript_new_script_pubkey $ mkScriptPubkey hash

convertScriptAll :: Array T.NativeScript -> Maybe NativeScript
convertScriptAll nss =
  nativeScript_new_script_all <<< mkScriptAll <<<
    packNativeScripts <$> for nss convertNativeScript

convertScriptAny :: Array T.NativeScript -> Maybe NativeScript
convertScriptAny nss =
  nativeScript_new_script_any <<< mkScriptAny <<<
    packNativeScripts <$> for nss convertNativeScript

convertScriptNOfK :: Int -> Array T.NativeScript -> Maybe NativeScript
convertScriptNOfK n nss =
  nativeScript_new_script_n_of_k <<< mkScriptNOfK n <<<
    packNativeScripts <$> for nss convertNativeScript

convertTimelockStart :: T.Slot -> NativeScript
convertTimelockStart (T.Slot slot) =
  nativeScript_new_timelock_start $ mkTimelockStart $ UInt.toInt slot

convertTimelockExpiry :: T.Slot -> NativeScript
convertTimelockExpiry (T.Slot slot) =
  nativeScript_new_timelock_expiry $ mkTimelockExpiry $ UInt.toInt slot

packNativeScripts :: Array NativeScript -> NativeScripts
packNativeScripts = _packNativeScripts containerHelper

foreign import mkScriptPubkey :: T.Ed25519KeyHash -> ScriptPubkey
foreign import _packNativeScripts :: ContainerHelper -> Array NativeScript -> NativeScripts
foreign import mkScriptAll :: NativeScripts -> ScriptAll
foreign import mkScriptAny :: NativeScripts -> ScriptAny
foreign import mkScriptNOfK :: Int -> NativeScripts -> ScriptNOfK
foreign import mkTimelockStart :: Int -> TimelockStart
foreign import mkTimelockExpiry :: Int -> TimelockExpiry
foreign import nativeScript_new_script_pubkey :: ScriptPubkey -> NativeScript
foreign import nativeScript_new_script_all :: ScriptAll -> NativeScript
foreign import nativeScript_new_script_any :: ScriptAny -> NativeScript
foreign import nativeScript_new_script_n_of_k :: ScriptNOfK -> NativeScript
foreign import nativeScript_new_timelock_start :: TimelockStart -> NativeScript
foreign import nativeScript_new_timelock_expiry :: TimelockExpiry -> NativeScript
