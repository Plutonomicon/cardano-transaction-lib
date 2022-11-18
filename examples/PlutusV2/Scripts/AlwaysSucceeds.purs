module Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

foreign import alwaysSucceedsV2 :: String

alwaysSucceedsScriptV2 :: Contract () Validator
alwaysSucceedsScriptV2 =
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceedsV2
    Validator <$> plutusScriptV2FromEnvelope envelope
