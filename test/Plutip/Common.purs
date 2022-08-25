module Test.Plutip.Common
  ( config
  , privateStakeKey
  ) where

import Prelude

import Contract.Wallet (privateKeyFromBytes)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.UInt (fromInt) as UInt
import Partial.Unsafe (unsafePartial)
import Plutip.Types (PlutipConfig)
import Types.RawBytes (hexToRawBytes)
import Wallet.Key (PrivateStakeKey)

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
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
  , suppressLogs: true
  , customLogger: Nothing
  }

privateStakeKey :: PrivateStakeKey
privateStakeKey = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes =<< hexToRawBytes
      "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
