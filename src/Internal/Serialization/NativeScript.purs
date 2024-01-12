module Ctl.Internal.Serialization.NativeScript
  ( convertNativeScript
  , convertNativeScripts
  ) where

import Prelude

import Cardano.Serialization.Lib
  ( nativeScript_newScriptAll
  , nativeScript_newScriptAny
  , nativeScript_newScriptNOfK
  , nativeScript_newScriptPubkey
  , nativeScript_newTimelockExpiry
  , nativeScript_newTimelockStart
  , scriptAll_new
  , scriptAny_new
  , scriptNOfK_new
  , scriptPubkey_new
  , timelockExpiry_newTimelockexpiry
  , timelockStart_newTimelockstart
  )
import Cardano.Serialization.Lib.Internal (packListContainer)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  ) as T
import Ctl.Internal.Serialization.Address (Slot(Slot)) as T
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash) as T
import Ctl.Internal.Serialization.Types (NativeScript, NativeScripts)
import Data.Int as Int
import Data.Newtype (unwrap)

convertNativeScripts :: Array T.NativeScript -> NativeScripts
convertNativeScripts = packListContainer <<< map convertNativeScript

-- | Note: unbounded recursion here.
convertNativeScript :: T.NativeScript -> NativeScript
convertNativeScript = case _ of
  T.ScriptPubkey keyHash -> convertScriptPubkey keyHash
  T.ScriptAll nss -> convertScriptAll nss
  T.ScriptAny nss -> convertScriptAny nss
  T.ScriptNOfK n nss -> convertScriptNOfK n nss
  T.TimelockStart slot -> convertTimelockStart slot
  T.TimelockExpiry slot -> convertTimelockExpiry slot

convertScriptPubkey :: T.Ed25519KeyHash -> NativeScript
convertScriptPubkey hash = do
  nativeScript_newScriptPubkey $ scriptPubkey_new (unwrap hash)

convertScriptAll :: Array T.NativeScript -> NativeScript
convertScriptAll nss =
  nativeScript_newScriptAll <<< scriptAll_new <<<
    packListContainer $ map convertNativeScript nss

convertScriptAny :: Array T.NativeScript -> NativeScript
convertScriptAny nss =
  nativeScript_newScriptAny <<< scriptAny_new <<<
    packListContainer $ map convertNativeScript nss

convertScriptNOfK :: Int -> Array T.NativeScript -> NativeScript
convertScriptNOfK n nss =
  nativeScript_newScriptNOfK <<< scriptNOfK_new (Int.toNumber n) <<<
    packListContainer $ map convertNativeScript nss

convertTimelockStart :: T.Slot -> NativeScript
convertTimelockStart (T.Slot slot) =
  nativeScript_newTimelockStart (timelockStart_newTimelockstart $ unwrap slot)

convertTimelockExpiry :: T.Slot -> NativeScript
convertTimelockExpiry (T.Slot slot) =
  nativeScript_newTimelockExpiry
    (timelockExpiry_newTimelockexpiry $ unwrap slot)
