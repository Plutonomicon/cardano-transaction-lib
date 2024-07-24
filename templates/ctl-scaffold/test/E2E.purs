-- | This module is used to serve the E2E tests to the headless browser.
module Scaffold.Test.E2E.Serve where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , mainnetConfig
  , testnetConfig
  )
import Contract.Monad (Contract)
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Data.Map (Map)
import Data.Map as Map
import Scaffold as Scaffold

main :: Effect Unit
main = do
  addLinks configs tests
  route configs tests

configs :: Map E2EConfigName (ContractParams /\ Maybe String)
configs = Map.fromFoldable
  [ "nami" /\ testnetConfig' "nami" /\ Nothing
  , "gero" /\ testnetConfig' "gerowallet" /\ Nothing
  , "flint" /\ testnetConfig' "flint" /\ Nothing
  , "eternl" /\ testnetConfig' "eternl" /\ Nothing
  , "lode" /\ testnetConfig' "LodeWallet" /\ Nothing
  , "nami-mock" /\ testnetConfig' "nami" /\ Just "nami"
  , "gero-mock" /\ testnetConfig' "gerowallet" /\ Just "gerowallet"
  , "flint-mock" /\ testnetConfig' "flint" /\ Just "flint"
  , "lode-mock" /\ testnetConfig' "LodeWallet" /\ Just "LodeWallet"
  -- Plutip cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ mainnetConfig' "nami" /\ Just "nami"
  , "plutip-gero-mock" /\ mainnetConfig' "gerowallet" /\ Just "gerowallet"
  , "plutip-flint-mock" /\ mainnetConfig' "flint" /\ Just "flint"
  , "plutip-lode-mock" /\ mainnetConfig' "LodeWallet" /\ Just "LodeWallet"
  ]
  where
  testnetConfig' :: String -> ContractParams
  testnetConfig' wallet =
    testnetConfig
      { walletSpec =
          Just $ ConnectToGenericCip30 wallet { cip95: false }
      }

  mainnetConfig' :: String -> ContractParams
  mainnetConfig' wallet =
    mainnetConfig
      { walletSpec =
          Just $ ConnectToGenericCip30 wallet { cip95: false }
      }

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  [ "Contract" /\ Scaffold.contract
  -- Add more `Contract`s here
  ]
