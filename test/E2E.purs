module Test.E2E (main) where

import Contract.Test.E2E (parseCliArgs, runE2ECommand)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = do
  options <- parseCliArgs
  launchAff_ do
    runE2ECommand options
