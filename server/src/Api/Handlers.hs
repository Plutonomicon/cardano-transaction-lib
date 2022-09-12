module Api.Handlers (
  applyArgs,
) where

import Plutus.V1.Ledger.Scripts (applyArguments)
import Types (AppliedScript (AppliedScript), ApplyArgsRequest (ApplyArgsRequest, args, script))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

applyArgs :: ApplyArgsRequest -> AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  AppliedScript $ applyArguments script args
