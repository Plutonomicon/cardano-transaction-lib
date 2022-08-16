module Contract.TextEnvelope
  ( module TextEnvelope
  , textEnvelopeBytes
  ) where

import Contract.Prelude

import Cardano.TextEnvelope (textEnvelopeBytes, TextEnvelopeType) as TE
import Cardano.TextEnvelope
  ( decodeTextEnvelope
  , printTextEnvelopeDecodeError
  , TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
  ) as TextEnvelope
import Contract.Monad (Contract)
import Types.ByteArray (ByteArray)

textEnvelopeBytes :: String -> TE.TextEnvelopeType -> Contract () ByteArray
textEnvelopeBytes json ty = liftAff $ TE.textEnvelopeBytes json ty
