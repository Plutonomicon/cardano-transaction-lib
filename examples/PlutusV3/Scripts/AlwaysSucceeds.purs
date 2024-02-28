module Ctl.Examples.PlutusV3.Scripts.AlwaysSucceeds
  ( alwaysSucceedsScriptV3
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV3FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysSucceedsScriptV3 :: Contract Validator
alwaysSucceedsScriptV3 = do
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceedsV3
    Validator <$> plutusScriptV3FromEnvelope envelope

alwaysSucceedsV3 :: String
alwaysSucceedsV3 =
  """
{
    "type": "PlutusScriptV3",
    "description": "",
    "cborHex": "484701010022280001"
}
"""
