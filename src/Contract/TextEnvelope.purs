module CTL.Contract.TextEnvelope
  ( module TextEnvelope
  , textEnvelopeBytes
  ) where

import CTL.Contract.Prelude

import CTL.Internal.Cardano.TextEnvelope
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
import CTL.Internal.Cardano.TextEnvelope (printTextEnvelopeDecodeError)
import CTL.Internal.Cardano.TextEnvelope (textEnvelopeBytes, TextEnvelopeType) as TE
import CTL.Contract.Monad (Contract)
import Data.Bifunctor (lmap)
import Effect.Exception (error)
import CTL.Internal.Types.ByteArray (ByteArray)

textEnvelopeBytes :: String -> TE.TextEnvelopeType -> Contract () ByteArray
textEnvelopeBytes json ty =
  liftEither $ lmap (error <<< printTextEnvelopeDecodeError) $
    TE.textEnvelopeBytes json ty
