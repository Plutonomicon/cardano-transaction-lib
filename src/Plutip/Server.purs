module Plutip.Server where

import Contract.Monad
import Data.Tuple.Nested
import Effect.Aff
import Node.ChildProcess
import Node.FS.Aff
import Plutip.Monad
import Plutip.Types
import Prelude
import Undefined

import Aeson
  ( class DecodeAeson
  , JsonDecodeError(..)
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toStringifiedNumbersJson
  , (.:)
  )
import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address (NetworkId(..))
import Control.Monad.Error.Class (withResource)
import Control.Monad.Logger.Class (error)
import Control.Monad.Logger.Trans (LoggerT(..), runLoggerT)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Control.MonadZero (empty)
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Posix.Signal (Signal(..))
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Helpers (logWithLevel)
import Node.Encoding as Encoding
import Node.Path (concat, dirname, sep)
import QueryM (ClientError(..))
import Plutip.Utils

type PlutipM (r :: Row Type) (a :: Type) = ReaderT PlutipConfig (Contract r) a

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path

-- main :: Effect Unit
-- main = launchAff_ do
--   runPlutipM { host: "localhost"
--              , port: UInt.fromInt 8081
--              , logLevel: Trace
--              } do
--     startPlutipCluster >>= liftEffect <<< log <<< show

runPlutipM
  :: forall (r :: Row Type) (a :: Type)
   . PlutipConfig
  -> (ConfigParams () -> ConfigParams r)
  -> PlutipM r a
  -> Aff a
runPlutipM plutipCfg enrich action = do
  withResource
    do
      startPlutipCluster plutipCfg $ ClusterStartupRequest
        { keysToGenerate: plutipCfg.distribution }
    (\_ -> void $ stopPlutipCluster plutipCfg)
    \startupResponse -> do
      response <- case startupResponse of
        ClusterStartupFailure _ -> do
          liftEffect $ throw "Failed to start up cluster"
        ClusterStartupSuccess response -> do
          pure response
      let
        contract =
          flip runReaderT plutipCfg action
      withResource (startOgmios plutipCfg response) stopOgmios \_ -> do
        withResource (startOgmiosDatumCache plutipCfg response)
          stopOgmiosDatumCache
          \_ -> do
            contractCfg <- mkContractConfig $ enrich $ ConfigParams
              { ogmiosConfig: plutipCfg.ogmiosConfig
              , datumCacheConfig: plutipCfg.ogmiosDatumCacheConfig
              , ctlServerConfig: plutipCfg.ctlServerConfig
              , networkId: undefined
              , slotConfig: defaultSlotConfig
              , logLevel: Trace
              , extraConfig: {}
              , wallet: Nothing -- TODO: some wallet that is able to use skeys
              }
            liftAff $ runContract contractCfg contract

startPlutipCluster
  :: PlutipConfig -> ClusterStartupRequest -> Aff StartClusterResponse
startPlutipCluster cfg startupRequest = do
  let url = mkServerEndpointUrl cfg "start"
  res <-
    liftAff
      ( Affjax.post Affjax.ResponseFormat.string url $ Just $ RequestBody.String
          $ stringifyAeson
          $ encodeAeson startupRequest
      )
      <#> either
        (Left <<< ClientHttpError)
        ( lmap ClientDecodeJsonError
            <<< (decodeAeson <=< parseJsonStringToAeson)
            <<< _.body
        )
  either (liftEffect <<< throw <<< show) pure res

stopPlutipCluster :: PlutipConfig -> Aff StopClusterResponse
stopPlutipCluster = undefined

startOgmios :: PlutipConfig -> ClusterStartupParameters -> Aff ChildProcess
startOgmios cfg params = liftEffect $ spawn "ogmios" ogmiosArgs
  defaultSpawnOptions
  where
  ogmiosArgs =
    [ "--host"
    , "0.0.0.0"
    , "--port"
    , UInt.toString cfg.ogmiosConfig.port
    , "--node-socket"
    , params.nodeSocketPath
    , "--node-config"
    , params.nodeConfigPath
    ]

stopOgmios :: ChildProcess -> Aff Unit
stopOgmios = liftEffect <<< kill SIGINT

-- TODO: use CLI arguments when https://github.com/mlabs-haskell/ogmios-datum-cache/issues/61 is resolved
startOgmiosDatumCache
  :: PlutipConfig -> ClusterStartupParameters -> Aff ChildProcess
startOgmiosDatumCache cfg params = do
  dir <- liftEffect tmpdir
  let
    configContents = fold
      [ "dbConnectionString = " <> undefined
      , "server.port = " <> UInt.toString cfg.ogmiosDatumCacheConfig.port
      , "ogmios.address = \"0.0.0.0\""
      , "ogmios.port = " <> UInt.toString cfg.ogmiosConfig.port
      , "blockFetcher.autoStart = true"
      , "blockFetcher.firstBlock.slot = 54066900"
      -- TODO: start from origin when https://github.com/mlabs-haskell/ogmios-datum-cache/pull/59 is merged
      ]
  writeTextFile Encoding.UTF8 (dir <> "/config.toml") configContents
  liftEffect $ spawn "ogmios-datum-cache" [] defaultSpawnOptions
    { cwd = Just dir }

stopOgmiosDatumCache :: ChildProcess -> Aff Unit
stopOgmiosDatumCache = liftEffect <<< kill SIGINT

mkClusterContractCfg
  :: forall (r :: Row Type)
   . ContractConfig r
  -> ClusterStartupParameters
  -> ContractConfig r
mkClusterContractCfg contractCfg clusterParams =
  flip (over ContractConfig) contractCfg
    \x -> x

data NetworkInfo = Mainnet | Testnet Int

type CtlServerConfig =
  { port :: Int
  , nodeSocketPath :: FilePath
  , networkId :: NetworkInfo
  }

runCtlServer :: CtlServerConfig -> Aff Unit
runCtlServer = undefined

type OgmiosConfig =
  { port :: Int
  , nodeSocketConfig :: FilePath
  , nodeSocketPath :: FilePath
  }

mkOgmiosConfig :: OgmiosConfig -> ClusterStartupParameters -> OgmiosConfig
mkOgmiosConfig ogmiosCfg { nodeSocketPath } = ogmiosCfg
  { nodeSocketConfig = concat [ dirname nodeSocketPath, "node.config" ]
  , nodeSocketPath = nodeSocketPath
  }

runOgmios :: OgmiosConfig -> Aff Unit
runOgmios = undefined

-- { ogmiosWs :: OgmiosWebSocket
-- , datumCacheWs :: DatumCacheWebSocket
-- , serverConfig :: ServerConfig
-- , wallet :: Maybe Wallet
-- -- should probably be more tightly coupled with a wallet
-- , usedTxOuts :: UsedTxOuts
-- , networkId :: NetworkId
-- , slotConfig :: SlotConfig
-- , logLevel :: LogLevel
