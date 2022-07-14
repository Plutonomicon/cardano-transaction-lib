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
      , PaymentSigningKeyShelley_ed25519
      , StakeSigningKeyShelley_ed25519
      )
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
  ) as TextEnvelope
import Contract.Monad (Contract)
import Types.ByteArray (ByteArray)

textEnvelopeBytes
  :: forall r. String -> TE.TextEnvelopeType -> Contract r ByteArray
textEnvelopeBytes json ty = liftAff $ TE.textEnvelopeBytes json ty
