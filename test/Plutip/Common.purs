module Test.Ctl.Plutip.Common
  ( config
  , privateStakeKey
  ) where

import Prelude

import Contract.Config (emptyHooks)
import Contract.Wallet (privateKeyFromBytes)
import Ctl.Internal.Plutip.Types (PlutipConfig)
import Ctl.Internal.Types.RawBytes (hexToRawBytes)
import Ctl.Internal.Wallet.Key (PrivateStakeKey)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (wrap)
import Data.UInt (fromInt) as UInt
import Partial.Unsafe (unsafePartial)

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
  , kupoConfig:
      { port: UInt.fromInt 1443
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
  , hooks: emptyHooks
  }

privateStakeKey :: PrivateStakeKey
privateStakeKey = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes =<< hexToRawBytes
      "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
