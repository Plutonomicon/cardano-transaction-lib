-- | This module implements a test suite that uses Cardano Testnet to automate running
-- | contracts in temporary, private networks.
module Test.Scaffold.Main (main, suite) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Testnet
  ( ContractTest
  , InitialUTxOs
  , defaultTestnetConfig
  , testTestnetContracts
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group, test)
import Scaffold (contract)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testTestnetContracts defaultTestnetConfig suite

suite :: TestPlanM ContractTest Unit
suite = do
  group "Project tests" do
    test "Print PubKey" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigNum.fromInt 5_000_000
          , BigNum.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          contract
