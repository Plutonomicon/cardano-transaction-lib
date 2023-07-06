module Ctl.Examples.PlutusV2.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV2
  , alwaysMintsPolicyV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers.LoadScript (loadScript)
import Effect.Exception (error)

alwaysMintsPolicyV2 :: Contract MintingPolicy
alwaysMintsPolicyV2 = PlutusMintingPolicy <$> alwaysMintsPolicyScriptV2

alwaysMintsPolicyScriptV2 :: Contract PlutusScript
alwaysMintsPolicyScriptV2 = do
  alwaysMintsV2 <- liftAff $ loadScript "always-mints-v2.plutus"
  liftMaybe (error "Error decoding alwaysMintsV2") do
    envelope <- decodeTextEnvelope alwaysMintsV2
    plutusScriptV2FromEnvelope envelope
