-- | This module is used to serve the E2E tests to the headless browser.
module Scaffold.Test.E2E.Serve where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , KnownWallet(Gero, Eternl, Lode)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
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
configs = map (map walletName) <$> Map.fromFoldable
  [ "gero" /\ testnetConfig' Gero /\ Nothing
  , "eternl" /\ testnetConfig' Eternl /\ Nothing
  , "lode" /\ testnetConfig' Lode /\ Nothing
  , "gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "lode-mock" /\ testnetConfig' Lode /\ Just Lode
  , "eternl-mock" /\ testnetConfig' Eternl /\ Just Eternl
  , "localnet-eternl-mock" /\ testnetConfig' Eternl /\ Just Eternl
  , "localnet-gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "localnet-lode-mock" /\ testnetConfig' Lode /\ Just Lode
  ]
  where
  testnetConfig' :: KnownWallet -> ContractParams
  testnetConfig' wallet =
    testnetConfig
      { walletSpec =
          Just $ ConnectToGenericCip30 (walletName wallet) { cip95: false }
      }

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  [ "Contract" /\ Scaffold.contract
  -- Add more `Contract`s here
  ]
