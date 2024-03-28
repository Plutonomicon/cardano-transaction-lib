module Ctl.Examples.PlutusV2.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV2
  , alwaysMintsPolicyV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

alwaysMintsPolicyV2 :: Contract MintingPolicy
alwaysMintsPolicyV2 = PlutusMintingPolicy <$> alwaysMintsPolicyScriptV2

alwaysMintsPolicyScriptV2 :: Contract PlutusScript
alwaysMintsPolicyScriptV2 = do
  liftMaybe (error "Error decoding alwaysMintsV2") do
    envelope <- decodeTextEnvelope alwaysMintsV2
    plutusScriptFromEnvelope envelope

alwaysMintsV2 :: String
alwaysMintsV2 =
  """
{
    "cborHex": "484701000022120011",
    "description": "always-mints",
    "type": "PlutusScriptV2"
}
"""
