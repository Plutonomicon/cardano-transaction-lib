module Plutip.Server
  ( runPlutipContract
  , withPlutipContractEnv
  , startPlutipCluster
  , stopPlutipCluster
  , startPlutipServer
  , stopChildProcessWithPort
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address (NetworkId(MainnetId))
import Contract.Monad
  ( Contract
  , ContractEnv(ContractEnv)
  , liftContractM
  , runContractInEnv
  )
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Either (Either(Left), either)
import Data.Foldable (sum)
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (foldMap, for, for_, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds), bracket)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.ChildProcess
  ( ChildProcess
  , defaultExecSyncOptions
  , defaultSpawnOptions
  , execSync
  , kill
  , spawn
  )
import Plutip.PortCheck (isPortAvailable)
import Plutip.Spawn
  ( NewOutputAction(Success, NoOp)
  , killOnExit
  , spawnAndWaitForOutput
  )
import Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , InitialUTxOs
  , PlutipConfig
  , PostgresConfig
  , PrivateKeyResponse(PrivateKeyResponse)
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  , InitialUTxODistribution
  )
import Plutip.Utils (tmpdir)
import Plutip.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , encodeDistribution
  , keyWallets
  , transferFundsFromEnterpriseToBase
  )
import QueryM
  ( ClientError(ClientDecodeJsonError, ClientHttpError)
  , stopQueryRuntime
  )
import QueryM as QueryM
import QueryM.ProtocolParameters as Ogmios
import QueryM.UniqueId (uniqueId)
import Type.Prelude (Proxy(Proxy))
import Types.UsedTxOuts (newUsedTxOuts)
import Wallet.Key (PrivatePaymentKey(PrivatePaymentKey))

-- | Run a single `Contract` in Plutip environment.
runPlutipContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (wallets -> Contract () a)
  -> Aff a
runPlutipContract cfg distr cont = withPlutipContractEnv cfg distr
  \env wallets ->
    runContractInEnv env (cont wallets)

-- | Provide a `ContractEnv` connected to Plutip.
-- | can be used to run multiple `Contract`s using `runContractInEnv`.
withPlutipContractEnv
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (ContractEnv () -> wallets -> Aff a)
  -> Aff a
withPlutipContractEnv plutipCfg distr cont = do
  configCheck plutipCfg
  withPlutipServer $
    withPlutipCluster \(ourKey /\ response) ->
      withPostgres response
        $ withOgmios response
        $ withOgmiosDatumCache response
        $ withCtlServer
        $ withContractEnv \env ->
            withWallets env ourKey response (cont env)
  where
  withPlutipServer :: Aff a -> Aff a
  withPlutipServer =
    bracket (startPlutipServer plutipCfg)
      (stopChildProcessWithPort plutipCfg.port) <<< const

  withPlutipCluster
    :: (PrivatePaymentKey /\ ClusterStartupParameters -> Aff a) -> Aff a
  withPlutipCluster cc = do
    let
      distrArray =
        encodeDistribution $
          ourInitialUtxos (encodeDistribution distr) /\
            distr
    for_ distrArray $ traverse_ \n -> when (n < BigInt.fromInt 1_000_000) do
      liftEffect $ throw $ "UTxO is too low: " <> BigInt.toString n <>
        ", must be at least 1_000_000 Lovelace"
    bracket
      (startPlutipCluster plutipCfg distrArray)
      (const $ void $ stopPlutipCluster plutipCfg)
      cc

  withPostgres :: ClusterStartupParameters -> Aff a -> Aff a
  withPostgres response =
    bracket (startPostgresServer plutipCfg.postgresConfig response)
      (stopChildProcessWithPort plutipCfg.postgresConfig.port) <<< const

  withOgmios :: ClusterStartupParameters -> Aff a -> Aff a
  withOgmios response =
    bracket (startOgmios plutipCfg response)
      (stopChildProcessWithPort plutipCfg.ogmiosConfig.port) <<< const

  withOgmiosDatumCache :: ClusterStartupParameters -> Aff a -> Aff a
  withOgmiosDatumCache response =
    bracket (startOgmiosDatumCache plutipCfg response)
      (stopChildProcessWithPort plutipCfg.ogmiosDatumCacheConfig.port) <<< const

  withCtlServer :: Aff a -> Aff a
  withCtlServer =
    bracket (startCtlServer plutipCfg)
      (stopChildProcessWithPort plutipCfg.ctlServerConfig.port) <<< const

  withWallets
    :: ContractEnv ()
    -> PrivatePaymentKey
    -> ClusterStartupParameters
    -> (wallets -> Aff a)
    -> Aff a
  withWallets env ourKey response cc = do
    wallets <- runContractInEnv
      (over wrap (_ { config { customLogger = Just (const $ pure unit) } }) env)
      do
        wallets <-
          liftContractM
            "Impossible happened: could not decode wallets. Please report as bug"
            $ decodeWallets distr response.privateKeys
        let walletsArray = keyWallets (Proxy :: Proxy distr) wallets
        transferFundsFromEnterpriseToBase ourKey walletsArray
        pure wallets
    cc wallets

  withContractEnv :: (ContractEnv () -> Aff a) -> Aff a
  withContractEnv = bracket (mkClusterContractEnv plutipCfg)
    (liftEffect <<< stopContractEnv)

  -- a version of Contract.Monad.stopContractEnv without a compile-time warning
  stopContractEnv :: ContractEnv () -> Effect Unit
  stopContractEnv env = stopQueryRuntime (unwrap env).runtime

-- | Throw an exception if `PlutipConfig` contains ports that are occupied.
configCheck :: PlutipConfig -> Aff Unit
configCheck cfg = do
  let
    services =
      [ cfg.port /\ "plutip-server"
      , cfg.ogmiosConfig.port /\ "ogmios"
      , cfg.ogmiosDatumCacheConfig.port /\ "ogmios-datum-cache"
      , cfg.ctlServerConfig.port /\ "ctl-server"
      , cfg.postgresConfig.port /\ "postgres"
      ]
  occupiedServices <- Array.catMaybes <$> for services \(port /\ service) -> do
    isPortAvailable port <#> if _ then Nothing else Just (port /\ service)
  unless (Array.null occupiedServices) do
    liftEffect $ throw $
      "Unable to run the following services, because the ports are occupied:\
      \\n" <> foldMap printServiceEntry occupiedServices
  where
  printServiceEntry (port /\ service) =
    "- " <> service <> " (port: " <> show (UInt.toInt port) <> ")\n"

-- | Start the plutip cluster, initializing the state with the given
-- | utxo distribution. Also initializes an extra payment key (aka
-- | `ourKey`) with some utxos for use with further plutip
-- | setup. `ourKey` has funds proportional to the total amount of the
-- | utxos in the passed distribution, so it can be used to handle
-- | transaction fees.
startPlutipCluster
  :: PlutipConfig
  -> InitialUTxODistribution
  -> Aff (PrivatePaymentKey /\ ClusterStartupParameters)
startPlutipCluster cfg keysToGenerate = do
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
                    { keysToGenerate }
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
  either (liftEffect <<< throw <<< show) pure res >>=
    case _ of
      ClusterStartupFailure _ -> do
        liftEffect $ throw "Failed to start up cluster"
      ClusterStartupSuccess response@{ privateKeys } ->
        case Array.uncons privateKeys of
          Nothing ->
            liftEffect $ throw $
              "Impossible happened: insufficient private keys provided by plutip. Please report as bug."
          Just { head: PrivateKeyResponse ourKey, tail } ->
            pure $ PrivatePaymentKey ourKey /\ response { privateKeys = tail }

-- | Calculate the initial utxos needed for `ourKey` to cover
-- | transaction costs for the given initial distribution
ourInitialUtxos :: InitialUTxODistribution -> InitialUTxOs
ourInitialUtxos utxoDistribution =
  let
    total = Array.foldr (sum >>> add) zero utxoDistribution
  in
    [ -- Take the total value of the utxos and add some extra on top
      -- of it to cover the possible transaction fees. Also make sure
      -- we don't request a 0 ada utxo
      total + BigInt.fromInt 1_000_000_000
    ]

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
  child <- spawnAndWaitForOutput "ogmios" ogmiosArgs defaultSpawnOptions
    $ pure Success
  liftEffect $ killOnExit child
  pure child
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
    defaultSpawnOptions
  liftEffect $ killOnExit p
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
    postgresSocket = workingDir <> "/postgres"
  liftEffect $ void $ execSync ("initdb " <> databaseDir) defaultExecSyncOptions
  pgChildProcess <- liftEffect $ spawn "postgres"
    [ "-D"
    , databaseDir
    , "-p"
    , UInt.toString pgConfig.port
    , "-h"
    , pgConfig.host
    , "-k"
    , postgresSocket
    ]
    defaultSpawnOptions
  liftEffect $ killOnExit pgChildProcess
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

-- | Kill a process and wait for it to stop listening on a specific port.
stopChildProcessWithPort :: UInt -> ChildProcess -> Aff Unit
stopChildProcessWithPort port childProcess = do
  stopChildProcess childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

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
      , "--from-origin"
      ]
  child <-
    spawnAndWaitForOutput "ogmios-datum-cache" arguments defaultSpawnOptions
      -- Wait for "Intersection found" string in the output
      $ String.indexOf (Pattern "Intersection found")
          >>> maybe NoOp (const Success)
  liftEffect $ killOnExit child
  pure child

mkClusterContractEnv
  :: PlutipConfig
  -> Aff (ContractEnv ())
mkClusterContractEnv plutipCfg = do
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff plutipCfg.logLevel
      QueryM.defaultDatumCacheWsConfig
        { port = plutipCfg.ogmiosDatumCacheConfig.port
        , host = plutipCfg.ogmiosDatumCacheConfig.host
        }
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff datumCacheWs plutipCfg.logLevel
    QueryM.defaultOgmiosWsConfig
      { port = plutipCfg.ogmiosConfig.port
      , host = plutipCfg.ogmiosConfig.host
      }
  usedTxOuts <- newUsedTxOuts
  pparams <- Ogmios.getProtocolParametersAff ogmiosWs plutipCfg.logLevel
  pure $ ContractEnv
    { config:
        { ctlServerConfig: plutipCfg.ctlServerConfig
        , ogmiosConfig: plutipCfg.ogmiosConfig
        , datumCacheConfig: plutipCfg.ogmiosDatumCacheConfig
        , networkId: MainnetId
        , logLevel: plutipCfg.logLevel
        , walletSpec: Nothing
        , customLogger: Nothing
        }
    , runtime:
        { ogmiosWs
        , datumCacheWs
        , wallet: Nothing
        , usedTxOuts
        , pparams
        }
    , extraConfig: {}
    }

startCtlServer :: PlutipConfig -> Aff ChildProcess
startCtlServer cfg = do
  let
    ctlServerArgs =
      [ "--port"
      , UInt.toString cfg.ctlServerConfig.port
      ]
  child <- spawnAndWaitForOutput "ctl-server" ctlServerArgs defaultSpawnOptions
    -- Wait for "Successfully connected to Ogmios" string in the output
    $ String.indexOf (Pattern "CTL server starting on port")
        >>> maybe NoOp (const Success)
  liftEffect $ killOnExit child
  pure child

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path
