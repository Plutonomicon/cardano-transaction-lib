-- | This module is used to serve the E2E tests to the headless browser.
module Scaffold.Test.E2E.Serve where

import Contract.Prelude

import Contract.Config (ContractParams)
import Contract.Monad (Contract)
import Contract.Test.E2E
  ( E2EConfigName
  , E2ETestName
  , addLinks
  , e2eConfigs
  , route
  )
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Effect.Exception (error)
import Scaffold as Scaffold

main :: Effect Unit
main = do
  configs <- liftEither $ lmap error mkConfigs
  addLinks configs tests
  route configs tests

mkConfigs :: Either String (Map E2EConfigName (ContractParams /\ Maybe String))
mkConfigs =
  e2eConfigs
    [ "eternl"
    , "gero"
    , "lode"
    , "eternl-mock"
    , "gero-mock"
    , "lode-mock"
    , "localnet-eternl-mock"
    , "localnet-gero-mock"
    , "localnet-lode-mock"
    ]

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  [ "Contract" /\ Scaffold.contract
  -- Add more `Contract`s here
  ]
