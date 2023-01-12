-- | Module to run `Test.Ctl.Plutip.Contract`s suite without Plutip, using
-- | an already running instance of Blockfrost.
module Test.Ctl.Blockfrost.Contract (main, suite) where

import Prelude

import Contract.Config
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , testnetBlockfrostDevConfig
  )
import Contract.Monad (launchAff_)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip (testContractsInEnv)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (joinWith)
import Data.Time.Duration (Minutes(Minutes), convertDuration)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Process (argv, exit)
import Test.Ctl.Plutip.Contract as Plutip
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Blockfrost.Contract --exec-args "BLOCKFROST_API_KEY PRIVATE_PAYMENT_FILE BACKUP_KEYS_DIR"`
main :: Effect Unit
main = do
  argv >>= case _ of
    [ _, apiKey, privateKey, backupKeys ] ->
      launchAff_ do
        interpretWithConfig
          defaultConfig
            { timeout = Just $ convertDuration $ 5.0 # Minutes }
          (suite apiKey privateKey backupKeys)
    _ -> do
      log $ joinWith "\n"
        [ "Wrong number of parameters provided."
        , "Usage:"
        , "  spago test --main Test.Ctl.Blockfrost.Contract --exec-args \"BLOCKFROST_API_KEY PRIVATE_PAYMENT_FILE BACKUP_KEYS_DIR\""
        , ""
        , "  BLOCKFROST_API_KEY   - Blockfrost preview API key"
        , "  PRIVATE_PAYMENT_FILE - PaymentSigningKeyShelley_ed25519 file, as produced by cardano-cli"
        , "  BACKUP_KEYS_DIR      - An existing directory to store generated funded wallets"
        ]
      exit 1

suite :: String -> String -> String -> TestPlanM (Aff Unit) Unit
suite apiKey privateKey backupKeys = do
  testContractsInEnv
    config
    backupKeys
    Plutip.suite
  where
  config =
    (testnetBlockfrostDevConfig (Just apiKey))
      { walletSpec = Just $ UseKeys
          (PrivatePaymentKeyFile privateKey)
          Nothing
      , suppressLogs = true
      }
