module Test.Ctl.E2E (main) where

import Prelude

import Contract.Test.E2E (parseCliArgs, runE2ECommand)
import Effect (Effect)
import Effect.Aff (launchAff_)

-- Run with `spago test --main Test.Ctl.E2E`
main :: Effect Unit
main = do
  options <- parseCliArgs
  launchAff_ do
    runE2ECommand options
