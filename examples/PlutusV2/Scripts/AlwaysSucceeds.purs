module Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers.LoadScript (loadScript)
import Effect.Exception (error)

alwaysSucceedsScriptV2 :: Contract Validator
alwaysSucceedsScriptV2 = do
  alwaysSucceedsV2 <- liftAff $ loadScript "always-succeeds-v2.plutus"
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceedsV2
    Validator <$> plutusScriptV2FromEnvelope envelope
