module Cardano.Types.ScriptHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.Serialization.Lib
  ( fromBytes
  , scriptHash_fromBech32
  , scriptHash_toBech32
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.TransactionMetadata (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray
  ( byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , hexToByteArray
  )
import Data.Either (Either(Right, Left))
import Data.Function (on)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

-- | blake2b-224 hash digests of serialized monetary scripts
newtype ScriptHash = ScriptHash Csl.ScriptHash

derive instance Newtype ScriptHash _

instance Eq ScriptHash where
  eq = eq `on` encodeCbor

instance Ord ScriptHash where
  compare = compare `on` encodeCbor

instance Show ScriptHash where
  show edkh = "(ScriptHash " <> byteArrayToHex (unwrap $ encodeCbor edkh) <> ")"

instance Arbitrary ScriptHash where
  arbitrary =
    unsafePartial $
      fromJust <<< decodeCbor <<< wrap <<< byteArrayFromIntArrayUnsafe
        <$> vectorOf 28 (chooseInt 0 255)

instance AsCbor ScriptHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData ScriptHash where
  toData = toData <<< unwrap <<< toBytes <<< unwrap

instance FromData ScriptHash where
  fromData (Bytes bytes) = decodeCbor $ wrap bytes
  fromData _ = Nothing

instance ToMetadata ScriptHash where
  toMetadata = toMetadata <<< toBytes <<< unwrap

instance FromMetadata ScriptHash where
  fromMetadata (Metadata.Bytes bytes) = decodeCbor $ wrap bytes
  fromMetadata _ = Nothing

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson ScriptHash where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded script hash") Right <<<
      caseAesonString Nothing
        (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson ScriptHash where
  encodeAeson sh = encodeAeson $ encodeCbor sh

-- | Decodes a script hash from its Bech32 representation
scriptHashFromBech32 :: Bech32String -> Maybe ScriptHash
scriptHashFromBech32 = map wrap <<< toMaybe <<< scriptHash_fromBech32

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will return `Nothing` if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
scriptHashToBech32Unsafe :: Partial => String -> ScriptHash -> Bech32String
scriptHashToBech32Unsafe prefix = unwrap >>> flip scriptHash_toBech32 prefix
