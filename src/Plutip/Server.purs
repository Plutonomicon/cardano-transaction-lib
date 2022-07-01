module Plutip.Server where

import Prelude

import Aeson
  ( decodeAeson
  , encodeAeson
  , getField
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toNumber
  , toObject
  )
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address (NetworkId(MainnetId))
import Contract.Monad (Contract, ContractConfig(ContractConfig), runContract)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.UInt as UInt
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds))
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers (fromJustEff, fromRightEff)
import Node.ChildProcess
  ( ChildProcess
  , defaultExecSyncOptions
  , defaultSpawnOptions
  , execSync
  , kill
  , spawn
  )
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (concat, dirname)
import Plutip.Spawn (NewOutputAction(Success, NoOp), spawnAndWaitForOutput)
import Plutip.Types
  ( class UtxoDistribution
  , ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , FilePath
  , PlutipConfig
  , PostgresConfig
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  , decodeWallets
  , encodeDistribution
  )
import Plutip.Utils (tmpdir)
import QueryM (ClientError(ClientDecodeJsonError, ClientHttpError))
import QueryM as QueryM
import QueryM.ProtocolParameters as Ogmios
import QueryM.UniqueId (uniqueId)
import Types.UsedTxOuts (newUsedTxOuts)

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path

runPlutipContract
  :: forall (a :: Type) (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (wallets -> Contract () a)
  -> Aff a
runPlutipContract plutipCfg distr contContract =
  withPlutipServer $
    withPlutipCluster \response ->
      withWallets response \wallets ->
        withPostgres response
          $ withOgmios response
          $ withOgmiosDatumCache response
          $ withCtlServer response do
              contractCfg <- mkClusterContractCfg plutipCfg
              liftAff $ runContract contractCfg (contContract wallets)
  where
  withPlutipServer =
    withResource (startPlutipServer plutipCfg) stopChildProcess <<< const
  withPlutipCluster cont = withResource (startPlutipCluster plutipCfg distr)
    (const $ void $ stopPlutipCluster plutipCfg)
    \startupResponse -> do
      case startupResponse of
        ClusterStartupFailure _ -> do
          liftEffect $ throw "Failed to start up cluster"
        ClusterStartupSuccess response -> do
          cont response
  withPostgres response =
    withResource (startPostgresServer plutipCfg.postgresConfig response)
      stopChildProcess <<< const
  withOgmios response =
    withResource (startOgmios plutipCfg response) stopChildProcess <<< const
  withOgmiosDatumCache response =
    withResource (startOgmiosDatumCache plutipCfg response)
      stopChildProcess <<< const
  withCtlServer response =
    withResource (startCtlServer plutipCfg response)
      stopChildProcess <<< const

  withWallets :: ClusterStartupParameters -> (wallets -> Aff a) -> Aff a
  withWallets response cont = case decodeWallets response.privateKeys of
    Nothing ->
      liftEffect $ throw $ "Impossible happened: unable to decode" <>
        " wallets from private keys. Please report as bug."
    Just wallets -> cont wallets

startPlutipCluster
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> Aff StartClusterResponse
startPlutipCluster cfg utxoDistribution = do
  let url = mkServerEndpointUrl cfg "start"
  res <- do
    response <- liftAff
      ( Affjax.request
          Affjax.defaultRequest
            { content = Just
                $ RequestBody.String
                $ stringifyAeson
                $ encodeAeson
                $ ClusterStartupRequest
                    { keysToGenerate: encodeDistribution utxoDistribution }
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
startOgmios cfg params = do
  -- We wait for any output, because CTL-server tries to connect to Ogmios
  -- repeatedly, and we can just wait for CTL-server to connect, instead of
  -- waiting for Ogmios first.
  spawnAndWaitForOutput "ogmios" ogmiosArgs defaultSpawnOptions
    $ pure Success
  where
  ogmiosArgs :: Array String
  ogmiosArgs =
    [ "--host"
    , cfg.ogmiosConfig.host
    , "--port"
    , UInt.toString cfg.ogmiosConfig.port
    , "--node-socket"
    , params.nodeSocketPath
    , "--node-config"
    , params.nodeConfigPath
    ]

stopChildProcess :: ChildProcess -> Aff Unit
stopChildProcess = liftEffect <<< kill SIGINT

startPlutipServer :: PlutipConfig -> Aff ChildProcess
startPlutipServer cfg = do
  p <- liftEffect $ spawn "plutip-server" [ "-p", UInt.toString cfg.port ]
    defaultSpawnOptions { detached = true }
  -- We are trying to call stopPlutipCluster endpoint to ensure that
  -- `plutip-server` has started.
  void
    $ recovering defaultRetryPolicy
        ([ \_ _ -> pure true ])
    $ const
    $ stopPlutipCluster cfg
  pure p

startPostgresServer
  :: PostgresConfig -> ClusterStartupParameters -> Aff ChildProcess
startPostgresServer pgConfig _ = do
  tmpDir <- liftEffect tmpdir
  randomStr <- liftEffect $ uniqueId ""
  let
    workingDir = tmpDir <> "/" <> randomStr
    databaseDir = workingDir <> "/postgres/data"
  liftEffect $ void $ execSync ("initdb " <> databaseDir) defaultExecSyncOptions
  pgChildProcess <- liftEffect $ spawn "postgres"
    [ "-D"
    , databaseDir
    , "-p"
    , UInt.toString pgConfig.port
    , "-h"
    , pgConfig.host
    , "-k"
    , workingDir <> "/postgres"
    ]
    defaultSpawnOptions
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    $ const
    $ liftEffect
    $ execSync
        ( "psql -h " <> pgConfig.host <> " -p " <> UInt.toString pgConfig.port
            <> " -d postgres"
        )
        defaultExecSyncOptions
  liftEffect $ void $ execSync
    ( "psql -h " <> pgConfig.host <> " -p " <> UInt.toString pgConfig.port
        <> " -d postgres"
        <> " -c \"CREATE ROLE "
        <> pgConfig.user
        <> " WITH LOGIN SUPERUSER CREATEDB PASSWORD '"
        <> pgConfig.password
        <> "';\""
    )
    defaultExecSyncOptions
  liftEffect $ void $ execSync
    ( "createdb -h " <> pgConfig.host <> " -p " <> UInt.toString pgConfig.port
        <> " -U "
        <> pgConfig.user
        <> " -O "
        <> pgConfig.user
        <> " "
        <> pgConfig.dbname
    )
    defaultExecSyncOptions
  pure pgChildProcess

startOgmiosDatumCache
  :: PlutipConfig
  -> ClusterStartupParameters
  -> Aff ChildProcess
startOgmiosDatumCache cfg _params = do
  apiKey <- liftEffect $ uniqueId "token"
  let
    arguments :: Array String
    arguments =
      [ "--server-api"
      , apiKey
      , "--server-port"
      , UInt.toString cfg.ogmiosDatumCacheConfig.port
      , "--ogmios-address"
      , cfg.ogmiosDatumCacheConfig.host
      , "--ogmios-port"
      , UInt.toString cfg.ogmiosConfig.port
      , "--db-port"
      , UInt.toString cfg.postgresConfig.port
      , "--db-host"
      , cfg.postgresConfig.host
      , "--db-user"
      , cfg.postgresConfig.user
      , "--db-name"
      , cfg.postgresConfig.dbname
      , "--db-password"
      , cfg.postgresConfig.password
      , "--use-latest"
      , "--origin"
      ]
  spawnAndWaitForOutput "ogmios-datum-cache" arguments defaultSpawnOptions
    -- Wait for "Intersection found" string in the output
    $ String.indexOf (Pattern "Intersection found")
        >>> maybe NoOp (const Success)

mkClusterContractCfg
  :: forall (r :: Row Type)
   . PlutipConfig
  -> Aff (ContractConfig ())
mkClusterContractCfg plutipCfg = do
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff plutipCfg.logLevel
    QueryM.defaultOgmiosWsConfig
      { port = plutipCfg.ogmiosConfig.port
      , host = plutipCfg.ogmiosConfig.host
      }
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff plutipCfg.logLevel
      QueryM.defaultDatumCacheWsConfig
        { port = plutipCfg.ogmiosDatumCacheConfig.port
        , host = plutipCfg.ogmiosDatumCacheConfig.host
        }
  usedTxOuts <- newUsedTxOuts
  pparams <- Ogmios.getProtocolParametersAff ogmiosWs plutipCfg.logLevel
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet: Nothing
    , usedTxOuts
    , serverConfig: plutipCfg.ctlServerConfig
    , networkId: MainnetId
    , logLevel: plutipCfg.logLevel
    , pparams
    }

data NetworkInfo = Mainnet | Testnet Int

type CtlServerConfig =
  { port :: Int
  , nodeSocketPath :: FilePath
  , networkId :: NetworkInfo
  }

startCtlServer :: PlutipConfig -> ClusterStartupParameters -> Aff ChildProcess
startCtlServer cfg params = do
  networkInfo <- liftEffect $ getNetworkInfoFromNodeConfig params.nodeConfigPath
  let
    ctlServerArgs =
      [ "--port"
      , UInt.toString cfg.ctlServerConfig.port
      , "--node-socket"
      , params.nodeSocketPath
      , "--ogmios-host"
      , cfg.ogmiosConfig.host
      , "--ogmios-port"
      , UInt.toString cfg.ogmiosConfig.port
      ] <> getNetworkInfoArgs networkInfo
  spawnAndWaitForOutput "ctl-server" ctlServerArgs defaultSpawnOptions
    -- Wait for "Successfully connected to Ogmios" string in the output
    $ String.indexOf (Pattern "Successfully connected to Ogmios")
        >>> maybe NoOp (const Success)

  where
  getNetworkInfoArgs :: NetworkInfo -> Array String
  getNetworkInfoArgs Mainnet =
    [ "--network-id", "mainnet" ]
  getNetworkInfoArgs (Testnet networkId) =
    [ "--network-id", Int.toStringAs Int.decimal networkId ]

getNetworkInfoFromNodeConfig :: FilePath -> Effect NetworkInfo
getNetworkInfoFromNodeConfig nodeConfigPath = do
  nodeConfigYaml <- readTextFile Encoding.UTF8 nodeConfigPath
  nodeConfigJson <- fromRightEff $ runExcept $ parseYAMLToJson nodeConfigYaml
  nodeConfigAeson <- fromJustEff "Not a json object" $ toObject $ jsonToAeson
    nodeConfigJson
  byronGenesisFile <- fromRightEff $ getField nodeConfigAeson
    "ByronGenesisFile"
  if getField nodeConfigAeson "RequiresNetworkMagic" == Right "RequiresNoMagic" then
    pure Mainnet
  else do
    byronGenesisJson <- fromRightEff =<< parseJsonStringToAeson <$> readTextFile
      Encoding.UTF8
      byronGenesisFile
    networkMagicString <- fromRightEff $ getNestedAeson byronGenesisJson
      [ "protocolConsts", "protocolMagic" ]
    Testnet <$> fromJustEff "Couldn't convert 'protocolMagic' String to Int"
      (Int.fromString =<< toNumber networkMagicString)

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
