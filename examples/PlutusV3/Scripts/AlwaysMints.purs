module Ctl.Examples.PlutusV3.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV3
  , alwaysMintsPolicyV3
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV3FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysMintsPolicyV3 :: Contract MintingPolicy
alwaysMintsPolicyV3 = PlutusMintingPolicy <$> alwaysMintsPolicyScriptV3

alwaysMintsPolicyScriptV3 :: Contract PlutusScript
alwaysMintsPolicyScriptV3 = do
  liftMaybe (error "Error decoding alwaysMintsV3") do
    envelope <- decodeTextEnvelope alwaysMintsV3
    plutusScriptV3FromEnvelope envelope

alwaysMintsV3 :: String
alwaysMintsV3 =
  """
{
    "cborHex": "4746010100222601",
    "description": "always-mints",
    "type": "PlutusScriptV3"
}
"""
