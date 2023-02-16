module Test.Ctl.E2E (main) where

import Prelude

import Contract.Test.E2E (parseCliArgs, runE2ECommand)
import Contract.Test.Utils (interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.Aff (launchAff)

-- Run with `spago test --main Test.Ctl.E2E`
main :: Effect Unit
main = do
  options <- parseCliArgs
  interruptOnSignal SIGINT =<< launchAff do
    runE2ECommand options
