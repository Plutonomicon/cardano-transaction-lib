module Ctl.Internal.Plutip.Server
  ( runPlutipContract
  , withPlutipContractEnv
  , startPlutipCluster
  , stopPlutipCluster
  , startPlutipServer
  ) where

import Prelude

import Aeson
  ( decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  )
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
import Control.Parallel (parSequence_)
import Effect.Ref as Ref
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Ctl.Internal.Plutip.Spawn
  ( NewOutputAction(Success, NextLine)
  , Stopped
  , spawn
  , spawnAndWaitForOutput
  )
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , InitialUTxODistribution
  , InitialUTxOs
  , PlutipConfig
  , PostgresConfig
  , PrivateKeyResponse(PrivateKeyResponse)
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  )
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Plutip.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , encodeDistribution
  , keyWallets
  , transferFundsFromEnterpriseToBase
  )
import Ctl.Internal.QueryM
  ( ClientError(ClientDecodeJsonError, ClientHttpError)
  , Logger
  , mkLogger
  , stopQueryRuntime
  )
import Ctl.Internal.QueryM as QueryM
import Ctl.Internal.QueryM.Logging (setupLogs)
import Ctl.Internal.QueryM.ProtocolParameters as Ogmios
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Ctl.Internal.Types.UsedTxOuts (newUsedTxOuts)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey(PrivatePaymentKey))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either)
import Data.Foldable (sum)
import Data.HTTP.Method as Method
import Data.Log.Message (Message)
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
import Effect.Aff (Aff, Milliseconds(Milliseconds), bracket, throwError, try)
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
  , onExit
  , StdIOBehaviour(Pipe, Ignore)
  , ignore
  , stdout
  )
import Node.Stream (destroy)
import Type.Prelude (Proxy(Proxy))

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
  bracket
    (liftEffect $ Ref.new mempty)
    await
    (withPlutipContractEnv' plutipCfg distr cont)
  where
  await waitersRef = do
    waiters <- liftEffect $ Ref.read waitersRef
    parSequence_ waiters

withPlutipContractEnv'
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (ContractEnv () -> wallets -> Aff a)
  -> Ref.Ref (Array (Aff Unit))
  -> Aff a
withPlutipContractEnv' plutipCfg distr cont waitersRef = do
  configCheck plutipCfg
  withPlutipServer $
    withPlutipCluster \(ourKey /\ response) ->
      withPostgres response
        $ withOgmios response
        $ withOgmiosDatumCache response
        $ withMCtlServer
        $ withContractEnv \env ->
            withWallets env ourKey response (cont env)
  where
  writeAwaitClosedChild :: ChildProcess /\ Stopped -> Aff ChildProcess
  writeAwaitClosedChild (child /\ stopped) = do
    liftEffect $ Ref.modify_ (_ <> [ stopped ]) waitersRef
    pure child

  withPlutipServer :: Aff a -> Aff a
  withPlutipServer =
    bracket (writeAwaitClosedChild =<< startPlutipServer plutipCfg)
      stopChildProcess <<< const

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
    bracket
      ( writeAwaitClosedChild =<< startPostgresServer plutipCfg.postgresConfig
          response
      )
      stopChildProcess <<< const

  withOgmios :: ClusterStartupParameters -> Aff a -> Aff a
  withOgmios response =
    bracket (writeAwaitClosedChild =<< startOgmios plutipCfg response)
      stopChildProcess <<< const

  withOgmiosDatumCache :: ClusterStartupParameters -> Aff a -> Aff a
  withOgmiosDatumCache response =
    bracket (writeAwaitClosedChild =<< startOgmiosDatumCache plutipCfg response)
      stopChildProcess <<< const

  withMCtlServer :: Aff a -> Aff a
  withMCtlServer = case plutipCfg.ctlServerConfig of
    Nothing -> identity
    Just config ->
      bracket (writeAwaitClosedChild =<< startCtlServer config.port)
        stopChildProcess <<< const

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
  withContractEnv | plutipCfg.suppressLogs = \contractEnvCont -> do
    -- if logs should be suppressed, setup the machinery and continue with
    -- the bracket
    { addLogEntry, suppressedLogger, printLogs } <-
      liftEffect $ setupLogs plutipCfg.logLevel plutipCfg.customLogger

    let configLogger = Just $ liftEffect <<< addLogEntry

    bracket (mkClusterContractEnv plutipCfg suppressedLogger configLogger)
      (liftEffect <<< stopContractEnv)
      $ contractEnvCont >>> try >=> case _ of
          Left err -> do
            liftEffect printLogs
            throwError err
          Right res -> pure res
  withContractEnv =
    -- otherwise, proceed with the env setup and provide a normal logger
    bracket
      ( mkClusterContractEnv plutipCfg
          (mkLogger plutipCfg.logLevel plutipCfg.customLogger)
          plutipCfg.customLogger
      )
      (liftEffect <<< stopContractEnv)

  -- a version of Contract.Monad.stopContractEnv without a compile-time warning
  stopContractEnv :: ContractEnv () -> Effect Unit
  stopContractEnv env = stopQueryRuntime (unwrap env).runtime

-- | Throw an exception if `PlutipConfig` contains ports that are occupied.
configCheck :: PlutipConfig -> Aff Unit
configCheck cfg = do
  let
    services :: Array (UInt /\ String)
    services =
      [ cfg.port /\ "plutip-server"
      , cfg.ogmiosConfig.port /\ "ogmios"
      , cfg.ogmiosDatumCacheConfig.port /\ "ogmios-datum-cache"
      , cfg.postgresConfig.port /\ "postgres"
      ] <> foldMap (pure <<< (_ /\ "ctl-server") <<< _.port) cfg.ctlServerConfig
  occupiedServices <- Array.catMaybes <$> for services \(port /\ service) -> do
    isPortAvailable port <#> if _ then Nothing else Just (port /\ service)
  unless (Array.null occupiedServices) do
    liftEffect $ throw $
      "Unable to run the following services, because the ports are occupied:\
      \\n" <> foldMap printServiceEntry occupiedServices
  where
  printServiceEntry :: UInt /\ String -> String
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

startOgmios
  :: PlutipConfig -> ClusterStartupParameters -> Aff (ChildProcess /\ Stopped)
startOgmios cfg params = do
  -- We wait for any output, because CTL-server tries to connect to Ogmios
  -- repeatedly, and we can just wait for CTL-server to connect, instead of
  -- waiting for Ogmios first.
  child /\ stopped <-
    spawnAndWaitForOutput "ogmios" ogmiosArgs
      (defaultSpawnOptions { stdio = [ Just Ignore, Just Pipe, Just Ignore ] })
      -- defaultSpawnOptions
      $ const
      $ pure Success
  liftEffect $ onExit child $ const $ destroy (stdout child)
  -- liftEffect $ 
  -- liftEffect $ end (stdout child)
  pure $ child /\ stopped
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

-- | Kill a child process
stopChildProcess :: ChildProcess -> Aff Unit
stopChildProcess child = liftEffect $ kill SIGINT child

startPlutipServer :: PlutipConfig -> Aff (ChildProcess /\ Stopped)
startPlutipServer cfg = do
  p <- spawn "plutip-server" [ "-p", UInt.toString cfg.port ]
    (defaultSpawnOptions { stdio = ignore })
  -- defaultSpawnOptions
  -- We are trying to call stopPlutipCluster endpoint to ensure that
  -- `plutip-server` has started.
  void
    $ recovering defaultRetryPolicy
        ([ \_ _ -> pure true ])
    $ const
    $ stopPlutipCluster cfg
  pure p

startPostgresServer
  :: PostgresConfig -> ClusterStartupParameters -> Aff (ChildProcess /\ Stopped)
startPostgresServer pgConfig _ = do
  tmpDir <- liftEffect tmpdir
  randomStr <- liftEffect $ uniqueId ""
  let
    workingDir = tmpDir <> "/" <> randomStr
    databaseDir = workingDir <> "/postgres/data"
    postgresSocket = workingDir <> "/postgres"
  liftEffect $ void $ execSync ("initdb " <> databaseDir) defaultExecSyncOptions
  pgChildProcess <- spawn "postgres"
    [ "-D"
    , databaseDir
    , "-p"
    , UInt.toString pgConfig.port
    , "-h"
    , pgConfig.host
    , "-k"
    , postgresSocket
    ]
    (defaultSpawnOptions { stdio = ignore })
  -- defaultSpawnOptions
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
  -> Aff (ChildProcess /\ Stopped)
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
  child /\ stopped <-
    spawnAndWaitForOutput "ogmios-datum-cache" arguments
      -- defaultSpawnOptions
      (defaultSpawnOptions { stdio = [ Just Ignore, Just Pipe, Just Ignore ] })
      -- Wait for "Intersection found" string in the output
      $ String.indexOf (Pattern "Intersection found")
          >>> maybe NextLine (const Success)
          >>> pure
  liftEffect $ onExit child $ const $ destroy (stdout child)
  -- liftEffect $ destroy (stdout child)
  pure $ child /\ stopped

mkClusterContractEnv
  :: PlutipConfig
  -> Logger
  -> Maybe (Message -> Aff Unit)
  -> Aff (ContractEnv ())
mkClusterContractEnv plutipCfg logger customLogger = do
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff logger
      QueryM.defaultDatumCacheWsConfig
        { port = plutipCfg.ogmiosDatumCacheConfig.port
        , host = plutipCfg.ogmiosDatumCacheConfig.host
        }
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff datumCacheWs logger
    QueryM.defaultOgmiosWsConfig
      { port = plutipCfg.ogmiosConfig.port
      , host = plutipCfg.ogmiosConfig.host
      }
  usedTxOuts <- newUsedTxOuts
  pparams <- Ogmios.getProtocolParametersAff ogmiosWs logger
  pure $ ContractEnv
    { config:
        { ctlServerConfig: plutipCfg.ctlServerConfig
        , ogmiosConfig: plutipCfg.ogmiosConfig
        , datumCacheConfig: plutipCfg.ogmiosDatumCacheConfig
        , networkId: MainnetId
        , logLevel: plutipCfg.logLevel
        , walletSpec: Nothing
        , customLogger: customLogger
        , suppressLogs: plutipCfg.suppressLogs
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

startCtlServer :: UInt -> Aff (ChildProcess /\ Stopped)
startCtlServer serverPort = do
  let ctlServerArgs = [ "--port", UInt.toString serverPort ]
  child /\ stopped <-
    spawnAndWaitForOutput "ctl-server" ctlServerArgs
      (defaultSpawnOptions { stdio = [ Just Ignore, Just Pipe, Just Ignore ] })
      -- defaultSpawnOptions
      -- Wait for "Successfully connected to Ogmios" string in the output
      $ String.indexOf (Pattern "CTL server starting on port")
          >>> maybe NextLine (const Success)
          >>> pure
  liftEffect $ onExit child $ const $ destroy (stdout child)
  -- liftEffect $ destroy (stdout child)
  pure $ child /\ stopped

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <> "/" <> path
