module Ctl.Internal.Serialization.Hash
  ( module X
  , scriptHashFromBytes
  , scriptHashToBytes
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.Serialization.Lib (fromBytes, nativeScript_hash, toBytes)
import Cardano.Serialization.Lib as CSL
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types.Ed25519KeyHash as X
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Cardano.Types.ScriptHash as X
import Cardano.Types.VRFKeyHash as X
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Serialization.Types (NativeScript)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.RawBytes (RawBytes, rawBytesToHex)
import Ctl.Internal.Types.TransactionMetadata (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , hexToByteArray
  )
import Data.Either (Either(Left, Right), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

scriptHashFromBytes = decodeCbor <<< wrap
scriptHashToBytes = encodeCbor
