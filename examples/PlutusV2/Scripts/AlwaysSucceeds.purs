module Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysSucceedsScriptV2 :: Contract PlutusScript
alwaysSucceedsScriptV2 = do
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceedsV2
    plutusScriptFromEnvelope envelope

alwaysSucceedsV2 :: String
alwaysSucceedsV2 =
  """
{
    "cborHex": "4e4d01000033222220051200120011",
    "description": "always-succeeds",
    "type": "PlutusScriptV2"
}
"""
