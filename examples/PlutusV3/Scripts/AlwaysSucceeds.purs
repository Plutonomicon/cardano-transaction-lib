module Ctl.Examples.PlutusV3.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV3
  ) where

import Contract.Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysSucceedsScriptV3 :: Contract PlutusScript
alwaysSucceedsScriptV3 =
  liftMaybe (error "Error decoding alwaysSucceedsV3") do
    envelope <- decodeTextEnvelope alwaysSucceedsV3
    plutusScriptFromEnvelope envelope

alwaysSucceedsV3 :: String
alwaysSucceedsV3 =
  """
  {
      "cborHex": "46450101002499",
      "description": "always-succeeds",
      "type": "PlutusScriptV3"
  }
  """
