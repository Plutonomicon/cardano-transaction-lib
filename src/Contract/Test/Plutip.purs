-- | This module contains everything needed for `Contract` testing in Plutip
-- | environment.
module Contract.Test.Plutip
  ( PlutipTest
  , PlutipConfig
  , PlutipTestPlan
  , testPlutipContracts
  , defaultPlutipConfig
  , module X
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , decodeAeson
  , encodeAeson
  , finiteNumber
  , parseJsonStringToAeson
  , stringifyAeson
  , toStringifiedNumbersJson
  , (.:)
  )
import Affjax (defaultRequest) as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Cardano.Types (PrivateKey, RawBytes(RawBytes))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PrivateKey as PrivateKey
import Contract.ClientError
  ( ClientError(ClientHttpError, ClientDecodeJsonError)
  )
import Contract.Config (Hooks, ServerConfig)
import Contract.Monad (ContractEnv, liftContractM, runContractInEnv)
import Contract.Monad (runContractInEnv) as X
import Contract.Prelude (fst, liftEither, wrap)
import Contract.Test (class UtxoDistribution, InitialUTxOs)
import Contract.Wallet (withKeyWallet) as X
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.WaitUntilSlot (waitNSlots)
import Ctl.Internal.Helpers (unsafeFromJust, (<</>>))
import Ctl.Internal.Service.Error (pprintClientError)
import Ctl.Internal.Spawn (ManagedProcess, isPortAvailable, spawn, stop)
import Ctl.Internal.Test.ContractTest
  ( ContractTest
  , ContractTestPlan(ContractTestPlan)
  )
import Ctl.Internal.Test.ContractTest (ContractTestPlan) as Server
import Ctl.Internal.Test.ContractTest (noWallet, sameWallets, withWallets) as X
import Ctl.Internal.Test.UtxoDistribution
  ( class UtxoDistribution
  , InitialUTxODistribution
  , InitialUTxOs
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , UtxoAmount
  , withStakeKey
  ) as X
import Ctl.Internal.Test.UtxoDistribution
  ( InitialUTxODistribution
  , decodeWallets
  , encodeDistribution
  , keyWallets
  , transferFundsFromEnterpriseToBase
  )
import Ctl.Internal.Testnet.Contract (execDistribution)
import Ctl.Internal.Testnet.Server
  ( makeClusterContractEnv
  , startKupo
  , startOgmios
  )
import Ctl.Internal.Testnet.Utils (after, runCleanup, whenError)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey(PrivatePaymentKey))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method as Method
import Data.Log.Level (LogLevel(Trace))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (for, for_, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (Aff, Milliseconds(Milliseconds), try)
import Effect.Aff (bracket) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (error, message, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mote (bracket) as Mote
import Mote.Monad (mapTest)
import Mote.TestPlanM (TestPlanM)
import Node.ChildProcess (defaultSpawnOptions)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(Proxy))

-- | Type synonym for backwards compatibility.
type PlutipTest = ContractTest
type PlutipTestPlan = Server.ContractTestPlan

-- | A config that is used to run tests on Plutip clusters.
-- | Note that the test suite starts the services on the specified ports.
type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , clusterConfig ::
      { slotLength :: Seconds
      , epochSize :: Maybe UInt
      , maxTxSize :: Maybe UInt
      , raiseExUnitsToMax :: Boolean
      }
  }

-- | A default value for `PlutipConfig` type.
defaultPlutipConfig :: PlutipConfig
defaultPlutipConfig =
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
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , suppressLogs: true
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.1
      , epochSize: Nothing
      , maxTxSize: Nothing
      , raiseExUnitsToMax: false
      }
  }

-- | Run several `Contract`s in tests in a (single) Plutip environment (plutip-server and cluster, kupo, etc.).
-- | NOTE: This uses `MoteT`s bracketing, and thus has the same caveats.
-- |       Namely, brackets are run for each of the top-level groups and tests
-- |       inside the bracket.
-- |       If you wish to only set up Plutip once, ensure all tests that are passed
-- |       to `testPlutipContracts` are wrapped in a single group.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md#testing-with-mote
testPlutipContracts
  :: PlutipConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
testPlutipContracts plutipCfg tp = do
  plutipTestPlan <- lift $ execDistribution tp
  runPlutipTestPlan plutipCfg plutipTestPlan

-- | Run a `ContractTestPlan` in a (single) Plutip environment.
-- | Supports wallet reuse - see docs on sharing wallet state between
-- | wallets in `doc/plutip-testing.md`.
runPlutipTestPlan
  :: PlutipConfig
  -> ContractTestPlan
  -> TestPlanM (Aff Unit) Unit
runPlutipTestPlan plutipCfg (ContractTestPlan runContractTestPlan) = do
  -- Modify tests to pluck out parts of a single combined distribution
  runContractTestPlan \distr tests -> do
    cleanupRef <- liftEffect $ Ref.new mempty
    -- Sets a single Mote bracket at the top level, it will be run for all
    -- immediate tests and groups
    bracket (startPlutipContractEnv plutipCfg distr cleanupRef)
      (runCleanup cleanupRef)
      $ flip mapTest tests \test { env, wallets, printLogs, clearLogs } -> do
          whenError printLogs (runContractInEnv env (test wallets))
          clearLogs
  where
  -- `MoteT`'s bracket doesn't support supplying the constructed resource into
  -- the main action, so we use a `Ref` to store and read the result.
  bracket
    :: forall (a :: Type) (b :: Type)
     . Aff a
    -> Aff Unit
    -> TestPlanM (a -> Aff b) Unit
    -> TestPlanM (Aff b) Unit
  bracket before' after' act = do
    resultRef <- liftEffect $ Ref.new (Left $ error "Plutip not initialized")
    let
      before = do
        res <- try $ before'
        liftEffect $ Ref.write res resultRef
        pure res
      after = const $ after'
    Mote.bracket { before, after } $ flip mapTest act \t -> do
      result <- liftEffect $ Ref.read resultRef >>= liftEither
      t result

-- | Provide a `ContractEnv` connected to Plutip.
-- | Can be used to run multiple `Contract`s using `runContractInEnv`.
-- | Resources which are allocated in the `Aff` computation must be de-allocated
-- | via the `Ref (Array (Aff Unit))` parameter, even if the computation did not
-- | succesfully complete.
-- Startup is implemented sequentially, rather than with nested `Aff.bracket`,
-- to allow non-`Aff` computations to occur between setup and cleanup.
startPlutipContractEnv
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> Ref (Array (Aff Unit))
  -> Aff
       { env :: ContractEnv
       , wallets :: wallets
       , printLogs :: Aff Unit
       , clearLogs :: Aff Unit
       }
startPlutipContractEnv plutipCfg distr cleanupRef = do
  configCheck plutipCfg
  tryWithReport startPlutipServer' "Could not start Plutip server"
  (ourKey /\ response) <- tryWithReport startPlutipCluster'
    "Could not start Plutip cluster"
  tryWithReport (startOgmios' response) "Could not start Ogmios"
  tryWithReport (startKupo' response) "Could not start Kupo"
  { env, printLogs, clearLogs } <- makeClusterContractEnv cleanupRef plutipCfg
  wallets <- mkWallets' env ourKey response
  void $ try $ liftEffect do
    for_ env.hooks.onClusterStartup \clusterParamsCb -> do
      clusterParamsCb
        { privateKeys: response.privateKeys <#> unwrap
        , nodeSocketPath: response.nodeSocketPath
        , nodeConfigPath: response.nodeConfigPath
        , privateKeysDirectory: response.keysDirectory
        }
  pure
    { env
    , wallets
    , printLogs
    , clearLogs
    }
  where
  tryWithReport
    :: forall (a :: Type)
     . Aff a
    -> String
    -> Aff a
  tryWithReport what prefix = do
    result <- try what
    case result of
      Left err -> throwError $ error $ prefix <> ": " <> message err
      Right result' -> pure result'

  startPlutipServer' :: Aff Unit
  startPlutipServer' =
    cleanupBracket
      cleanupRef
      (startPlutipServer plutipCfg)
      (stopChildProcessWithPort plutipCfg.port)
      (const $ checkPlutipServer plutipCfg)

  startPlutipCluster'
    :: Aff (PrivatePaymentKey /\ ClusterStartupParameters)
  startPlutipCluster' = do
    let
      distrArray =
        encodeDistribution $
          ourInitialUtxos (encodeDistribution distr) /\
            distr
    for_ distrArray $ traverse_ \n -> when (n < BigNum.fromInt 1_000_000) do
      liftEffect $ throw $ "UTxO is too low: " <> BigNum.toString n <>
        ", must be at least 1_000_000 Lovelace"
    cleanupBracket
      cleanupRef
      (startPlutipCluster plutipCfg distrArray)
      (const $ void $ stopPlutipCluster plutipCfg)
      pure

  startOgmios' :: ClusterStartupParameters -> Aff Unit
  startOgmios' response =
    void
      $ after (startOgmios plutipCfg response)
      $ stopChildProcessWithPort plutipCfg.ogmiosConfig.port

  startKupo' :: ClusterStartupParameters -> Aff Unit
  startKupo' response =
    void
      $ after (startKupo plutipCfg response cleanupRef)
      $ fst
          >>> stopChildProcessWithPort plutipCfg.kupoConfig.port

  mkWallets'
    :: ContractEnv
    -> PrivatePaymentKey
    -> ClusterStartupParameters
    -> Aff wallets
  mkWallets' env ourKey response = do
    runContractInEnv
      env { customLogger = Just (\_ _ -> pure unit) }
      do
        wallets <-
          liftContractM
            "Impossible happened: could not decode wallets. Please report as bug"
            $ decodeWallets distr (coerce response.privateKeys)
        let walletsArray = keyWallets (Proxy :: Proxy distr) wallets
        void $ waitNSlots BigNum.one
        transferFundsFromEnterpriseToBase ourKey walletsArray
        pure wallets

startPlutipServer :: PlutipConfig -> Aff ManagedProcess
startPlutipServer cfg = do
  spawn "plutip-server" [ "-p", UInt.toString cfg.port ]
    defaultSpawnOptions
    Nothing

-- | Start the plutip cluster, initializing the state with the given
-- | UTxO distribution. Also initializes an extra payment key (aka
-- | `ourKey`) with some UTxOs for use with further plutip
-- | setup. `ourKey` has funds proportional to the total amount of the
-- | UTxOs in the passed distribution, so it can be used to handle
-- | transaction fees.
startPlutipCluster
  :: PlutipConfig
  -> InitialUTxODistribution
  -> Aff (PrivatePaymentKey /\ ClusterStartupParameters)
startPlutipCluster cfg keysToGenerate = do
  let
    url = mkServerEndpointUrl cfg "start"
    -- TODO: Non-default values for `slotLength` and `epochSize` break staking
    -- rewards, see https://github.com/mlabs-haskell/plutip/issues/149
    epochSize = fromMaybe (UInt.fromInt 80) cfg.clusterConfig.epochSize
  res <- do
    response <- liftAff
      ( Affjax.request
          Affjax.defaultRequest
            { content = Just
                $ RequestBody.String
                $ stringifyAeson
                $ encodeAeson
                $ ClusterStartupRequest
                    { keysToGenerate
                    , epochSize
                    , slotLength: cfg.clusterConfig.slotLength
                    , maxTxSize: cfg.clusterConfig.maxTxSize
                    , raiseExUnitsToMax: cfg.clusterConfig.raiseExUnitsToMax
                    }
            , responseFormat = Affjax.ResponseFormat.string
            , headers = [ Header.ContentType (wrap "application/json") ]
            , url = url
            , method = Left Method.POST
            }
      )
    pure $ response # either
      (Left <<< ClientHttpError)
      \{ body } -> lmap (ClientDecodeJsonError body)
        $ (decodeAeson <=< parseJsonStringToAeson) body
  either (liftEffect <<< throw <<< pprintClientError) pure res >>=
    case _ of
      ClusterStartupFailure reason -> do
        liftEffect $ throw
          $ "Failed to start up cluster. Reason: "
              <> show reason
      ClusterStartupSuccess response@{ privateKeys } ->
        case Array.uncons privateKeys of
          Nothing ->
            liftEffect $ throw $
              "Impossible happened: insufficient private keys provided by plutip. Please report as bug."
          Just { head: PrivateKeyResponse ourKey, tail } ->
            pure $ PrivatePaymentKey ourKey /\ response { privateKeys = tail }

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
      \{ body } -> lmap (ClientDecodeJsonError body)
        $ (decodeAeson <=< parseJsonStringToAeson)
            body
  either (liftEffect <<< throw <<< show) pure res

-- Similar to `Aff.bracket`, except cleanup is pushed onto a stack to be run
-- later.
cleanupBracket
  :: forall (a :: Type) (b :: Type)
   . Ref (Array (Aff Unit))
  -> Aff a
  -> (a -> Aff Unit)
  -> (a -> Aff b)
  -> Aff b
cleanupBracket cleanupRef before after action = do
  Aff.bracket
    before
    (\res -> liftEffect $ Ref.modify_ ([ after res ] <> _) cleanupRef)
    action

-- | Kill a process and wait for it to stop listening on a specific port.
stopChildProcessWithPort :: UInt -> ManagedProcess -> Aff Unit
stopChildProcessWithPort port childProcess = do
  stop childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

checkPlutipServer :: PlutipConfig -> Aff Unit
checkPlutipServer cfg = do
  -- We are trying to call stopPlutipCluster endpoint to ensure that
  -- `plutip-server` has started.
  void
    $ recovering defaultRetryPolicy
        ([ \_ _ -> pure true ])
    $ const
    $ stopPlutipCluster cfg

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

type ErrorMessage = String
data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage

data StopClusterRequest = StopClusterRequest

derive instance Generic StopClusterResponse _

instance EncodeAeson StopClusterRequest where
  encodeAeson _ = encodeAeson ([] :: Array Int)

instance Show StopClusterResponse where
  show = genericShow

instance DecodeAeson StopClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    obj .: "tag" >>= case _ of
      "StopClusterSuccess" -> pure StopClusterSuccess
      "StopClusterFailure" -> do
        failure <- obj .: "contents"
        StopClusterFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

type FilePath = String

type ClusterStartupParameters =
  { privateKeys :: Array PrivateKeyResponse
  , nodeSocketPath :: FilePath
  , nodeConfigPath :: FilePath
  , keysDirectory :: FilePath
  }

newtype PrivateKeyResponse = PrivateKeyResponse PrivateKey

derive instance Newtype PrivateKeyResponse _
derive instance Generic PrivateKeyResponse _

instance Show PrivateKeyResponse where
  show _ = "(PrivateKeyResponse \"<private key>\")"

instance DecodeAeson PrivateKeyResponse where
  decodeAeson json = do
    cborStr <- decodeAeson json
    cborBytes <- note err $ hexToByteArray cborStr
    PrivateKeyResponse <$> note err
      (PrivateKey.fromRawBytes (RawBytes cborBytes))
    where
    err :: JsonDecodeError
    err = TypeMismatch "PrivateKey"

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTxODistribution
  , epochSize :: UInt
  , slotLength :: Seconds
  , maxTxSize :: Maybe UInt
  , raiseExUnitsToMax :: Boolean
  }

instance EncodeAeson ClusterStartupRequest where
  encodeAeson
    ( ClusterStartupRequest
        { keysToGenerate
        , epochSize
        , slotLength: Seconds slotLength
        , maxTxSize
        , raiseExUnitsToMax
        }
    ) = encodeAeson
    { keysToGenerate
    , epochSize
    , slotLength: unsafeFromJust "instance EncodeAeson ClusterStartupRequest" $
        finiteNumber slotLength
    , maxTxSize
    , raiseExUnitsToMax
    }

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces
  | NodeConfigNotFound

derive instance Generic ClusterStartupFailureReason _

instance Show ClusterStartupFailureReason where
  show = genericShow

instance DecodeAeson ClusterStartupFailureReason where
  decodeAeson aeson = do
    decodeAeson aeson >>= case _ of
      "ClusterIsRunningAlready" -> do
        pure ClusterIsRunningAlready
      "NegativeLovelaces" -> pure NegativeLovelaces
      "NodeConfigNotFound" -> pure NodeConfigNotFound
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

data StartClusterResponse
  = ClusterStartupFailure ClusterStartupFailureReason
  | ClusterStartupSuccess ClusterStartupParameters

derive instance Generic StartClusterResponse _

instance Show StartClusterResponse where
  show = genericShow

instance DecodeAeson StartClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    obj .: "tag" >>= case _ of
      "ClusterStartupSuccess" -> do
        contents <- obj .: "contents"
        ClusterStartupSuccess <$> decodeAeson contents
      "ClusterStartupFailure" -> do
        failure <- obj .: "contents"
        ClusterStartupFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

-- | Calculate the initial UTxOs needed for `ourKey` to cover
-- | transaction costs for the given initial distribution
ourInitialUtxos :: InitialUTxODistribution -> InitialUTxOs
ourInitialUtxos utxoDistribution =
  let
    total = Array.foldr (\e acc -> unsafePartial $ fold e # append acc)
      BigNum.zero
      utxoDistribution
  in
    [ -- Take the total value of the UTxOs and add some extra on top
      -- of it to cover the possible transaction fees. Also make sure
      -- we don't request a 0 ada UTxO
      unsafePartial $ append total (BigNum.fromInt 1_000_000)
    ]

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <</>> path

-- | Throw an exception if `PlutipConfig` contains ports that are occupied.
configCheck :: PlutipConfig -> Aff Unit
configCheck cfg =
  checkPortsAreFree
    [ { port: cfg.port, service: "plutip-server" }
    , { port: cfg.ogmiosConfig.port, service: "ogmios" }
    , { port: cfg.kupoConfig.port, service: "kupo" }
    ]

-- | Throw an exception if any of the given ports is occupied.
checkPortsAreFree :: Array { port :: UInt, service :: String } -> Aff Unit
checkPortsAreFree ports = do
  occupiedServices <- Array.catMaybes <$> for ports \{ port, service } -> do
    isPortAvailable port <#> if _ then Nothing else Just (port /\ service)
  unless (Array.null occupiedServices) do
    liftEffect $ throw
      $
        "Unable to run the following services, because the ports are occupied:\
        \\n"
          <> foldMap printServiceEntry occupiedServices
  where
  printServiceEntry :: UInt /\ String -> String
  printServiceEntry (port /\ service) =
    "- " <> service <> " (port: " <> show (UInt.toInt port) <> ")\n"
