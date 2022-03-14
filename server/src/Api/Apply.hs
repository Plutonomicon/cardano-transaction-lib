module Api.Apply (
  applyScriptArgs,
) where

import Types (
  AppM,
  AppliedScript,
  ApplyArgsRequest (ApplyArgsRequest),
 )

applyScriptArgs :: ApplyArgsRequest -> AppM AppliedScript
applyScriptArgs ApplyArgsRequest {} = do
  undefined
