module Contract.TextEnvelope
  ( module TextEnvelope
  , liftEitherTextEnvelopeDecodeError
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Cardano.TextEnvelope (TextEnvelopeDecodeError)
import Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  , plutusScriptV2FromEnvelope
  , printTextEnvelopeDecodeError
  ) as TextEnvelope
import Data.Bifunctor (lmap)
import Effect.Exception (error)

liftEitherTextEnvelopeDecodeError
  :: forall a. Either TextEnvelopeDecodeError a -> Contract () a
liftEitherTextEnvelopeDecodeError =
  liftEither <<< lmap (error <<< TextEnvelope.printTextEnvelopeDecodeError)
