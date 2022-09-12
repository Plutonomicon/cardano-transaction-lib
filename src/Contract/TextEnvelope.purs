module Contract.TextEnvelope
  ( module TextEnvelope
  , textEnvelopeBytes
  ) where

import Contract.Prelude

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
import Cardano.TextEnvelope (printTextEnvelopeDecodeError)
import Cardano.TextEnvelope (textEnvelopeBytes, TextEnvelopeType) as TE
import Contract.Monad (Contract)
import Data.Bifunctor (lmap)
import Effect.Exception (error)
import Types.ByteArray (ByteArray)

textEnvelopeBytes :: String -> TE.TextEnvelopeType -> Contract () ByteArray
textEnvelopeBytes json ty =
  liftEither $ lmap (error <<< printTextEnvelopeDecodeError) $
    TE.textEnvelopeBytes json ty
