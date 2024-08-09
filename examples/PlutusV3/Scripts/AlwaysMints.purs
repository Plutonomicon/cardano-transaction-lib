module Ctl.Examples.PlutusV3.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV3
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysMintsPolicyScriptV3 :: Contract PlutusScript
alwaysMintsPolicyScriptV3 =
  liftMaybe (error "Error decoding alwaysMintsV3") do
    envelope <- decodeTextEnvelope alwaysMintsV3
    plutusScriptFromEnvelope envelope

alwaysMintsV3 :: String
alwaysMintsV3 =
  """
  {
      "cborHex": "46450101002499",
      "description": "always-mints",
      "type": "PlutusScriptV3"
  }
  """
