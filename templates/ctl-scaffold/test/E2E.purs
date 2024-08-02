-- | This module is used to serve the E2E tests to the headless browser.
module Scaffold.Test.E2E.Serve where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , KnownWallet(Nami, Gero, Flint, Eternl, Lode)
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
  [ "nami" /\ testnetConfig' Nami /\ Nothing
  , "gero" /\ testnetConfig' Gero /\ Nothing
  , "flint" /\ testnetConfig' Flint /\ Nothing
  , "eternl" /\ testnetConfig' Eternl /\ Nothing
  , "lode" /\ testnetConfig' Lode /\ Nothing
  , "nami-mock" /\ testnetConfig' Nami /\ Just Nami
  , "gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "flint-mock" /\ testnetConfig' Flint /\ Just Flint
  , "lode-mock" /\ testnetConfig' Lode /\ Just Lode
  , "plutip-nami-mock" /\ testnetConfig' Nami /\ Just Nami
  , "plutip-gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "plutip-flint-mock" /\ testnetConfig' Flint /\ Just Flint
  , "plutip-lode-mock" /\ testnetConfig' Lode /\ Just Lode
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
