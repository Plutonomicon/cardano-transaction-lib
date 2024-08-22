-- | Module to run `Test.Ctl.Testnet.Contract`s suite without Testnet, using
-- | an already running instance of Blockfrost (preview).
-- |
-- | Use `npm run blockfrost-test` to run.
module Test.Ctl.Blockfrost.Contract (main) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (launchAff_)
import Contract.Test.Blockfrost (executeContractTestsWithBlockfrost)
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Test.Ctl.Integration as IntegrationTest
import Test.Ctl.Testnet.Contract as Testnet
import Test.Spec.Runner (defaultConfig) as TestSpec

main :: Effect Unit
main = launchAff_ do
  executeContractTestsWithBlockfrost
    TestSpec.defaultConfig { timeout = Just $ Milliseconds 1000000.0 }
    testnetConfig { suppressLogs = true }
    do
      Testnet.suite
      IntegrationTest.stakingSuite
