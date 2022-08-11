module Api.Handlers (
  applyArgs,
) where

import Plutus.V1.Ledger.Scripts (applyArguments)
import Types (AppM, AppliedScript (AppliedScript), ApplyArgsRequest (ApplyArgsRequest, args, script))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $ applyArguments script args
