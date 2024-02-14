module Cardano.Types.Ed25519KeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.Serialization.Lib
  ( ed25519KeyHash_fromBech32
  , ed25519KeyHash_toBech32
  , fromBytes
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.PlutusData (PlutusData(Bytes))
import Ctl.Internal.FfiHelpers (partialToMaybe)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.RawBytes (RawBytes, rawBytesToHex)
import Ctl.Internal.Types.TransactionMetadata (TransactionMetadatum(Bytes)) as Metadata
import Data.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe, hexToByteArray)
import Data.Either (Either(Left), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype Ed25519KeyHash = Ed25519KeyHash Csl.Ed25519KeyHash

derive instance Newtype Ed25519KeyHash _

instance Eq Ed25519KeyHash where
  eq = eq `on` ed25519KeyHashToBytes

instance Ord Ed25519KeyHash where
  compare = compare `on` ed25519KeyHashToBytes

instance Show Ed25519KeyHash where
  show edkh =
    "(Ed25519KeyHash $ unsafePartial $ fromJust $ ed25519KeyHashFromBech32 "
      <> show (ed25519KeyHashToBech32 "pool" edkh)
      <> ")"

instance AsCbor Ed25519KeyHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData Ed25519KeyHash where
  toData = toData <<< unwrap <<< ed25519KeyHashToBytes

instance FromData Ed25519KeyHash where
  fromData (Bytes kh) = ed25519KeyHashFromBytes kh
  fromData _ = Nothing

instance ToMetadata Ed25519KeyHash where
  toMetadata = toMetadata <<< ed25519KeyHashToBytes

instance FromMetadata Ed25519KeyHash where
  fromMetadata (Metadata.Bytes kh) = ed25519KeyHashFromBytes kh
  fromMetadata _ = Nothing

-- This is needed for `ApplyArgs`.
instance DecodeAeson Ed25519KeyHash where
  -- ed25519KeyHashFromBech32 goes from Bech32String directly although this
  -- feels unsafe.
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected Plutus BuiltinByteString")
    ( note (TypeMismatch "Invalid Ed25519KeyHash") <<< ed25519KeyHashFromBytes
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToByteArray
    )

instance EncodeAeson Ed25519KeyHash where
  encodeAeson = encodeAeson <<< rawBytesToHex <<< ed25519KeyHashToBytes

instance Arbitrary Ed25519KeyHash where
  arbitrary =
    unsafePartial fromJust <<< ed25519KeyHashFromBytes <<<
      byteArrayFromIntArrayUnsafe <$> vectorOf 28 (chooseInt 0 255)

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
ed25519KeyHashToBech32Unsafe ∷ Partial => String → Ed25519KeyHash → Bech32String
ed25519KeyHashToBech32Unsafe prefix kh = ed25519KeyHash_toBech32 (unwrap kh)
  prefix

ed25519KeyHashToBytes :: Ed25519KeyHash -> RawBytes
ed25519KeyHashToBytes = wrap <<< toBytes <<< unwrap

ed25519KeyHashFromBytes :: ByteArray -> Maybe Ed25519KeyHash
ed25519KeyHashFromBytes = map wrap <<< fromBytes

ed25519KeyHashFromBech32 :: Bech32String -> Maybe Ed25519KeyHash
ed25519KeyHashFromBech32 = map wrap <<< toMaybe <<< ed25519KeyHash_fromBech32

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
ed25519KeyHashToBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
ed25519KeyHashToBech32 prefix kh = partialToMaybe \_ ->
  ed25519KeyHashToBech32Unsafe prefix kh
