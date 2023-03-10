-- | An executable test suite that runs `Test.Scaffold.Main.suite` with
-- | Blockfrost.
-- | Use `npm run blockfrost-test` to run.
module Scaffold.Test.Blockfrost (main) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (launchAff_)
import Contract.Test.Blockfrost (executeContractTestsWithBlockfrost)
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Test.Scaffold.Main (suite)
import Test.Spec.Runner (defaultConfig) as TestSpec

main :: Effect Unit
main = launchAff_ do
  executeContractTestsWithBlockfrost
    TestSpec.defaultConfig { timeout = Just $ Milliseconds 1000000.0 }
    testnetConfig { suppressLogs = true }
    suite
