module Scaffold.Test.Main (main) where

import Contract.Prelude

import Contract.Monad as Contract.Monad
import Contract.Test.Plutip as Contract.Test.Plutip
import Contract.Wallet as Contract.Wallet
import Data.BigInt as BigInt
import Data.UInt as UInt
import Scaffold as Scaffold

main :: Effect Unit
main = Contract.Monad.launchAff_ $ do
  let
    distribution :: Contract.Test.Plutip.InitialUTxOs
    distribution =
      [ BigInt.fromInt 5_000_000
      , BigInt.fromInt 2_000_000_000
      ]
  Contract.Test.Plutip.runPlutipContract config distribution \alice ->
    Contract.Wallet.withKeyWallet alice $ do
      Scaffold.contract

config :: Contract.Test.Plutip.PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig: Just
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing
  , suppressLogs: false
  }
