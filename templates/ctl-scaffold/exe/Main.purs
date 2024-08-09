-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Scaffold as Scaffold

main :: Effect Unit
main = Contract.Monad.launchAff_
  $ void
  $ Contract.Monad.runContract contractParams
  $ Scaffold.contract

contractParams :: Contract.Config.ContractParams
contractParams =
  Contract.Config.testnetConfig
    { walletSpec =
        Just $ Contract.Config.ConnectToGenericCip30 "nami" { cip95: false }
    }
