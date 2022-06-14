module Plutip.Server where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad (Contract, ContractConfig(ContractConfig), runContract)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left), either)
import Data.Foldable (intercalate)
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, kill, spawn)
import Node.Encoding as Encoding
import Node.FS.Aff (writeTextFile)
import Node.Path (concat, dirname)
import Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , FilePath
  , PlutipConfig
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  )
import Plutip.Utils (tmpdir)
import QueryM (ClientError(..))
import QueryM as QueryM
import Types.UsedTxOuts (newUsedTxOuts)
import Undefined (undefined)

type PlutipM (r :: Row Type) (a :: Type) = ReaderT PlutipConfig (Contract r) a

liftContract
  :: forall (r :: Row Type)
   . Contract r ~> PlutipM r
liftContract = ReaderT <<< const

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path

runPlutipM
  :: forall (a :: Type)
   . PlutipConfig
  -> PlutipM () a
  -> Aff a
runPlutipM plutipCfg action = do
  withResource (startPlutipCluster plutipCfg)
    (const $ void $ stopPlutipCluster plutipCfg)
    \startupResponse -> do
      response <- case startupResponse of
        ClusterStartupFailure _ -> do
          liftEffect $ throw "Failed to start up cluster"
        ClusterStartupSuccess response -> do
          pure response
      let
        contract =
          flip runReaderT plutipCfg action
      withResource (startOgmios plutipCfg response) stopOgmios $ const do
        withResource (startOgmiosDatumCache plutipCfg response)
          stopOgmiosDatumCache $ const do
          contractCfg <- mkClusterContractCfg plutipCfg response
          liftAff $ runContract contractCfg contract

startPlutipCluster
  :: PlutipConfig -> Aff StartClusterResponse
startPlutipCluster cfg = do
  let url = mkServerEndpointUrl cfg "start"
  res <- do
    response <- liftAff
      ( Affjax.request
          Affjax.defaultRequest
            { content = Just
                $ RequestBody.String
                $ stringifyAeson
                $ encodeAeson
                $ ClusterStartupRequest { keysToGenerate: cfg.distribution }
            , responseFormat = Affjax.ResponseFormat.string
            , headers = [ Header.ContentType (wrap "application/json") ]
            , url = url
            , method = Left Method.POST
            }
      )
    pure $ response # either
      (Left <<< ClientHttpError)
      ( lmap ClientDecodeJsonError
          <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )
  either (liftEffect <<< throw <<< show) pure res

stopPlutipCluster :: PlutipConfig -> Aff StopClusterResponse
stopPlutipCluster cfg = do
  let url = mkServerEndpointUrl cfg "stop"
  res <- do
    response <- liftAff
      ( Affjax.request
          Affjax.defaultRequest
            { content = Just
                $ RequestBody.String
                $ stringifyAeson
                $ encodeAeson
                $ StopClusterRequest
            , responseFormat = Affjax.ResponseFormat.string
            , headers = [ Header.ContentType (wrap "application/json") ]
            , url = url
            , method = Left Method.POST
            }
      )
    pure $ response # either
      (Left <<< ClientHttpError)
      ( lmap ClientDecodeJsonError
          <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )
  either (liftEffect <<< throw <<< show) pure res

startOgmios :: PlutipConfig -> ClusterStartupParameters -> Aff ChildProcess
startOgmios cfg params = liftEffect $ spawn "ogmios" ogmiosArgs
  defaultSpawnOptions
  where
  ogmiosArgs :: Array String
  ogmiosArgs =
    [ "--host"
    , "localhost"
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
startOgmiosDatumCache cfg _params = do
  dir <- liftEffect tmpdir
  let
    dbString :: String
    dbString = intercalate " "
      [ "host=localhost"
      , "port=" <> UInt.toString cfg.ogmiosDatumCachePostgresConfig.port
      , "user=" <> cfg.ogmiosDatumCachePostgresConfig.user
      , "dbname=" <> cfg.ogmiosDatumCachePostgresConfig.dbname
      , "password=" <> cfg.ogmiosDatumCachePostgresConfig.password
      ]

    configContents :: String
    configContents = intercalate "\n"
      [ "dbConnectionString = \"" <> dbString <> "\""
      , "server.port = " <> UInt.toString cfg.ogmiosDatumCacheConfig.port
      , "ogmios.address = \"localhost\""
      , "ogmios.port = " <> UInt.toString cfg.ogmiosConfig.port
      , "blockFetcher.autoStart = true"
      , "blockFetcher.firstBlock.slot = 0"
      -- TODO: start from origin when https://github.com/mlabs-haskell/ogmios-datum-cache/pull/59 is merged
      ]
  writeTextFile Encoding.UTF8 (dir <> "/config.toml") configContents
  liftEffect $ spawn "ogmios-datum-cache" [] defaultSpawnOptions
    { cwd = Just dir }

stopOgmiosDatumCache :: ChildProcess -> Aff Unit
stopOgmiosDatumCache = liftEffect <<< kill SIGINT

mkClusterContractCfg
  :: forall (r :: Row Type)
   . PlutipConfig
  -> ClusterStartupParameters
  -> Aff (ContractConfig ())
mkClusterContractCfg plutipCfg _clusterParams = do
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff plutipCfg.logLevel
    QueryM.defaultOgmiosWsConfig
      { port = plutipCfg.ogmiosConfig.port
      , host = "localhost"
      }
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff plutipCfg.logLevel
      QueryM.defaultDatumCacheWsConfig
        { port = plutipCfg.ogmiosDatumCacheConfig.port
        , host = "localhost"
        }
  usedTxOuts <- newUsedTxOuts
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet: Nothing -- TODO: use KeyWallet when it's ready
    , usedTxOuts
    , serverConfig: QueryM.defaultServerConfig
    , networkId: TestnetId
    , logLevel: plutipCfg.logLevel
    }

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
