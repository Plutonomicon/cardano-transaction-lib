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
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad (Contract, ContractConfig(ContractConfig), runContract)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String.CodeUnits as String
import Data.Traversable (for)
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
import Effect.Console (log)
import Effect.Exception (error, throw)
import Helpers (fromJustEff, fromRightEff, liftM)
import Node.Buffer as Bf
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
import Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , FilePath
  , PostgresConfig
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  , PlutipConfig
  )
import Plutip.Utils (tmpdir)
import QueryM (ClientError(ClientDecodeJsonError, ClientHttpError))
import QueryM as QueryM
import QueryM.UniqueId (uniqueId)
import Serialization (privateKeyFromBytes)
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes(RawBytes))
import Types.UsedTxOuts (newUsedTxOuts)
import Wallet (Wallet(KeyListWallet))
import Wallet.KeyList (KeyListWallet)
import Wallet.KeyList as KeyList

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

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
runPlutipM plutipCfg action =
  withPlutipServer $
    withPlutipCluster \response ->
      withPostgres response
        $ withOgmios response
        $ withOgmiosDatumCache response
        $ withCtlServer response do
            contractCfg <- mkClusterContractCfg plutipCfg response
            liftAff $ runContract contractCfg contract
  where
  withPlutipServer =
    withResource (startPlutipServer plutipCfg) stopChildProcess <<< const
  withPlutipCluster cont = withResource (startPlutipCluster plutipCfg)
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
  contract = flip runReaderT plutipCfg action

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
  liftEffect $ log =<< Bf.toString Encoding.UTF8 =<< execSync
    ( "psql -h " <> pgConfig.host <> " -p " <> UInt.toString pgConfig.port
        <> " -d postgres"
        <> " -c \"CREATE ROLE "
        <> pgConfig.user
        <> " WITH LOGIN SUPERUSER CREATEDB PASSWORD '"
        <> pgConfig.password
        <> "';\""
    )
    defaultExecSyncOptions
  liftEffect $ log =<< Bf.toString Encoding.UTF8 =<< execSync
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
  :: PlutipConfig -> ClusterStartupParameters -> Aff ChildProcess
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
      ]
  liftEffect $ spawn "ogmios-datum-cache" arguments defaultSpawnOptions

mkClusterContractCfg
  :: forall (r :: Row Type)
   . PlutipConfig
  -> ClusterStartupParameters
  -> Aff (ContractConfig ())
mkClusterContractCfg plutipCfg params = do
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
  wallet <- mkKeyListWallet params
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet: Just $ KeyListWallet wallet
    , usedTxOuts
    , serverConfig: plutipCfg.ctlServerConfig
    , networkId: TestnetId
    , logLevel: plutipCfg.logLevel
    }

mkKeyListWallet :: ClusterStartupParameters -> Aff KeyListWallet
mkKeyListWallet { privateKeys } = do
  cslPrivateKeys <- for privateKeys \pkString -> do
    let splitted = String.splitAt 4 pkString
    unless (splitted.before == "5820") do
      liftEffect $ throw $ "Incorrect PrivateKey format: " <> show pkString
    byteArray <- liftM (error "Unable to decode KeyWallet") $
      hexToByteArray splitted.after
    maybe (liftEffect $ throw "Unable to construct PrivateKey") pure
      $ privateKeyFromBytes (RawBytes byteArray)
  pure $ KeyList.mkKeyListWallet cslPrivateKeys

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
      ] <> getNetworkInfoArgs networkInfo
  liftEffect $ spawn "ctl-server" ctlServerArgs defaultSpawnOptions
  where
  getNetworkInfoArgs :: NetworkInfo -> Array String
  getNetworkInfoArgs Mainnet = []
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
