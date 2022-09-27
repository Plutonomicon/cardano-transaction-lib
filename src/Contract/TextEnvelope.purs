module Contract.TextEnvelope
  ( module TextEnvelope
  , textEnvelopeBytes
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , decodeTextEnvelope
  , printTextEnvelopeDecodeError
  ) as TextEnvelope
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelopeType, textEnvelopeBytes) as TE
import Ctl.Internal.Cardano.TextEnvelope (printTextEnvelopeDecodeError)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Bifunctor (lmap)
import Effect.Exception (error)

textEnvelopeBytes :: String -> TE.TextEnvelopeType -> Contract () ByteArray
textEnvelopeBytes json ty =
  liftEither $ lmap (error <<< printTextEnvelopeDecodeError) $
    TE.textEnvelopeBytes json ty
