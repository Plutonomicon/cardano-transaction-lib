module Contract.TextEnvelope
  ( module TextEnvelope
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
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
  ) as TextEnvelope
import Data.Bifunctor (lmap)
import Effect.Exception (error)
