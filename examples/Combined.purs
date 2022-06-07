-- | This module allows you to run all of the current examples, with a delay
-- | in between each. This is helpful to ensure that changes you introduce
-- | don't break existing functionality. If an example fails, you can run it
-- | individually afterwards to diagnose the problem
-- |
-- | To run an individual example, either edit the `ps-entrypoint` variable
-- | in the Makefile and run `make run-dev`, or issue the following commands:
-- |
-- | ```
-- | > spago bundle-module -m <ENTRYPOINT> --to output.js
-- | > BROWSER_RUNTIME=1 webpack-dev-server --progress
-- | ```
-- |
module Examples.Combined (main) where

import Contract.Prelude

import Data.Array (intersperse)
import Effect.Aff (launchAff_, delay)
import Examples.AlwaysMints as AlwaysMints
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.Datums as Datums
import Examples.Nami as Nami
import Examples.Pkh2Pkh as Pkh2Pkh

main :: Effect Unit
main = launchAff_ $ sequence_ $ intersperse sleep
  [ AlwaysMints.contract
  , Datums.contract
  , Nami.contract
  , Pkh2Pkh.contract
  , AlwaysSucceeds.contract
  ]
  where
  sleepWithMsg :: Aff Unit
  sleepWithMsg = log ("Sleeping for " <> show delayMillis) *> sleep

  -- Wait to run the next contract
  sleep :: Aff Unit
  sleep = delay $ wrap delayMillis

  delayMillis :: Number
  delayMillis = 5_000.0
