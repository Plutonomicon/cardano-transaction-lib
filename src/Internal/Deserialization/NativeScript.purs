module Ctl.Internal.Deserialization.NativeScript
  ( convertNativeScript
  , decodeNativeScript
  ) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch))
import Ctl.Internal.Cardano.Types.NativeScript as T
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , containerHelper
  )
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Ctl.Internal.Serialization.Types
  ( NativeScript
  , ScriptAll
  , ScriptAny
  , ScriptNOfK
  , ScriptPubkey
  , TimelockExpiry
  , TimelockStart
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Data.ByteArray (ByteArray)
import Data.Either (Either, note)
import Data.Newtype (wrap)

type ConvertNativeScript (r :: Type) =
  { scriptPubkey :: ScriptPubkey -> r
  , scriptAll :: ScriptAll -> r
  , scriptAny :: ScriptAny -> r
  , scriptNOfK :: ScriptNOfK -> r
  , timelockStart :: TimelockStart -> r
  , timelockExpiry :: TimelockExpiry -> r
  }

decodeNativeScript :: ByteArray -> Either JsonDecodeError T.NativeScript
decodeNativeScript scriptBytes = do
  nativeScript <-
    flip note (fromBytes $ wrap scriptBytes) $
      TypeMismatch "decodeNativeScript: from_bytes() call failed"
  pure $ convertNativeScript nativeScript

convertNativeScript :: NativeScript -> T.NativeScript
convertNativeScript ns = _convertNativeScript
  { scriptPubkey: T.ScriptPubkey <<< scriptPubkey_addr_keyhash
  , scriptAll: T.ScriptAll <<< map convertNativeScript <<< scriptAllScripts
      containerHelper
  , scriptAny: T.ScriptAny <<< map convertNativeScript <<< scriptAnyScripts
      containerHelper
  , scriptNOfK: T.ScriptNOfK <$> scriptNOfK_n <*>
      (map convertNativeScript <<< scriptNOfKScripts containerHelper)
  , timelockStart: T.TimelockStart <<< Slot <<< timelockStart_slot
  , timelockExpiry: T.TimelockExpiry <<< Slot <<< timelockExpiry_slot
  }
  ns

foreign import _convertNativeScript
  :: ConvertNativeScript T.NativeScript -> NativeScript -> T.NativeScript

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
