module Ctl.Examples.PlutusV2.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV2
  , alwaysMintsPolicyV2
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

foreign import alwaysMintsV2 :: String

alwaysMintsPolicyV2 :: Contract MintingPolicy
alwaysMintsPolicyV2 = PlutusMintingPolicy <$> alwaysMintsPolicyScriptV2

alwaysMintsPolicyScriptV2 :: Contract PlutusScript
alwaysMintsPolicyScriptV2 =
  liftMaybe (error "Error decoding alwaysMintsV2") do
    envelope <- decodeTextEnvelope alwaysMintsV2
    plutusScriptV2FromEnvelope envelope
