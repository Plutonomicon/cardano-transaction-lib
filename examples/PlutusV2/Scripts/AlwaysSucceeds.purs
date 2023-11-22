module Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysSucceedsScriptV2 :: Contract Validator
alwaysSucceedsScriptV2 = do
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceedsV2
    Validator <$> plutusScriptV2FromEnvelope envelope

alwaysSucceedsV2 :: String
alwaysSucceedsV2 =
  """
{
    "cborHex": "4e4d01000033222220051200120011",
    "description": "always-succeeds",
    "type": "PlutusScriptV2"
}
"""
