{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutus.Instances () where

import Codec.CBOR.Decoding qualified as CBOR (decodeBytes)
import Codec.CBOR.Write qualified as CBOR (toStrictByteString)
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as CBOR (decode, deserialiseOrFail, encode)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Aeson (Value (String), withText)
import Data.Aeson.Types qualified as Aeson (Parser)
import Data.ByteString qualified as BS (ByteString)
import Data.ByteString.Base16 qualified as Base16 (decode, encode)
import Data.ByteString.Lazy qualified as BSL (fromStrict)
import Data.Text qualified as Text (Text)
import Data.Text.Encoding qualified as TE (decodeUtf8, encodeUtf8)
import Flat qualified as Flat (Flat, flat, unflat)
import Plutus.V1.Ledger.Api qualified as Ledger (Data)
import Plutus.V1.Ledger.Scripts qualified as Ledger (Script (Script))

--------------------------------------------------------------------------------
-- JSON instances for `Ledger.Data`
--------------------------------------------------------------------------------

instance FromJSON Ledger.Data where
  parseJSON = decodeSerialise

instance ToJSON Ledger.Data where
  toJSON = Aeson.String . encodeSerialise

--------------------------------------------------------------------------------
-- JSON instances for `Ledger.Script`
--
-- The JSON instances for `Script` are partially hand-written rather than going
-- via the `Serialise` instance directly. The reason for this is to *avoid* the
-- size checks that are in place in the `Serialise` instance. These are only
-- useful for deserialisation checks on-chain, whereas the JSON instances are
-- used for e.g. transmitting validation events, which often include scripts
-- with the data arguments applied (which can be very big!).
--
-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Plutus/V1/Ledger/Scripts.hs#L208
--------------------------------------------------------------------------------

instance FromJSON Ledger.Script where
  parseJSON value = do
    SerialiseViaFlat p <- decodeSerialise value
    pure $ Ledger.Script p

instance ToJSON Ledger.Script where
  toJSON (Ledger.Script p) =
    Aeson.String $ encodeSerialise (SerialiseViaFlat p)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Newtype for to provide `Serialise` instances for types with a `Flat`
-- instance that just encodes the flat-serialized value as a CBOR bytestring.
--
-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Plutus/V1/Ledger/Scripts.hs#L120
newtype SerialiseViaFlat a = SerialiseViaFlat a

instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
  encode (SerialiseViaFlat a) = CBOR.encode (Flat.flat a)
  decode =
    CBOR.decodeBytes
      >>= either (fail . show) (pure . SerialiseViaFlat)
        . Flat.unflat

-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Data/Aeson/Extras.hs#L30
decodeByteString :: Aeson.Value -> Aeson.Parser BS.ByteString
decodeByteString =
  Aeson.withText
    "ByteString"
    (either fail pure . Base16.decode . TE.encodeUtf8)

-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Data/Aeson/Extras.hs#L36
decodeSerialise :: Serialise a => Aeson.Value -> Aeson.Parser a
decodeSerialise =
  decodeByteString
    >=> either (fail . show) pure . CBOR.deserialiseOrFail . BSL.fromStrict

-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Data/Aeson/Extras.hs#L24
encodeByteString :: BS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

-- Taken from plutus-ledger-api:
-- https://github.com/input-output-hk/plutus/blob/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-ledger-api/src/Data/Aeson/Extras.hs#L33
encodeSerialise :: Serialise a => a -> Text.Text
encodeSerialise = encodeByteString . CBOR.toStrictByteString . CBOR.encode
