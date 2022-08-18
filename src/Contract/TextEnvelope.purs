module Contract.TextEnvelope
  ( module TextEnvelope
  , textEnvelopeBytes
  ) where

import Contract.Prelude

import Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , decodeTextEnvelope
  , printTextEnvelopeDecodeError
  ) as TextEnvelope
import Cardano.TextEnvelope (TextEnvelopeType, textEnvelopeBytes) as TE
import Contract.Monad (Contract)
import Types.ByteArray (ByteArray)

textEnvelopeBytes :: String -> TE.TextEnvelopeType -> Contract () ByteArray
textEnvelopeBytes json ty = liftAff $ TE.textEnvelopeBytes json ty
