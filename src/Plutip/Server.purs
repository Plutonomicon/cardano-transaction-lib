module Plutip.Server where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Monad
  ( ConfigParams(..)
  , Contract
  , ContractConfig(..)
  , LogLevel(..)
  , defaultSlotConfig
  , mkContractConfig
  , runContract
  )
import Control.Monad.Error.Class (withResource)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..))
import Data.Newtype (over, wrap)
import Data.Posix.Signal (Signal(..))
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
  , ClusterStartupRequest(..)
  , FilePath
  , PlutipConfig
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  )
import Plutip.Utils (tmpdir)
import QueryM (ClientError(..))
import Undefined (undefined)

type PlutipM (r :: Row Type) (a :: Type) = ReaderT PlutipConfig (Contract r) a

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path

runPlutipM
  :: forall (r :: Row Type) (a :: Type)
   . PlutipConfig
  -> (ConfigParams () -> ConfigParams r)
  -> PlutipM r a
  -> Aff a
runPlutipM plutipCfg enrich action = do
  withResource
    do
      startPlutipCluster plutipCfg
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
              , networkId: undefined -- TODO
              , slotConfig: defaultSlotConfig
              , logLevel: Trace
              , extraConfig: {}
              , wallet: Nothing -- TODO: some wallet that is able to use skeys
              }
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
