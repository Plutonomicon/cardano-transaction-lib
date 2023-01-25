module Ctl.Internal.Plutip.Server
  ( runPlutipContract
  , withPlutipContractEnv
  , startPlutipCluster
  , stopPlutipCluster
  , startPlutipServer
  , checkPlutipServer
  , stopChildProcessWithPort
  , testPlutipContracts
  , withWallets
  , noWallet
  , testContractsInEnv
  , PlutipTest
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address
  ( NetworkId(MainnetId)
  , getWalletAddresses
  , ownPaymentPubKeysHashes
  )
import Contract.Config (ContractParams)
import Contract.Hashing (publicKeyHash)
import Contract.Log (logTrace')
import Contract.Monad
  ( Contract
  , ContractEnv
  , liftContractM
  , liftedE
  , runContractInEnv
  , withContractEnv
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  , submitTxFromConstraints
  )
import Contract.Utxos (utxosAt)
import Contract.Value (valueToCoin')
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key
  ( keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , publicKeyFromPrivateKey
  )
import Contract.Wallet.KeyFile (privatePaymentKeyToFile, privateStakeKeyToFile)
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.State (State, execState, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (censor, execWriterT, tell)
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.Monad
  ( buildBackend
  , getLedgerConstants
  , stopContractEnv
  )
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Deserialization.Keys (freshPrivateKey)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess
  , NewOutputAction(Success, NoOp)
  , OnSignalRef
  , cleanupOnSigint
  , cleanupTmpDir
  , removeOnSignal
  , spawn
  , stop
  )
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , InitialUTxODistribution
  , InitialUTxOs
  , PlutipConfig
  , PrivateKeyResponse(PrivateKeyResponse)
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  , UtxoAmount
  )
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Plutip.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , encodeDistribution
  , keyWallets
  , transferFundsFromEnterpriseToBase
  )
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Ctl.Internal.Plutus.Types.Value (Value, lovelaceValueOf)
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Service.Error
  ( ClientError(ClientDecodeJsonError, ClientHttpError)
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ScriptLookups (mkUnbalancedTx, unspentOutputs)
import Ctl.Internal.Types.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustSpendPubKeyOutput
  )
import Ctl.Internal.Types.UsedTxOuts (newUsedTxOuts)
import Ctl.Internal.Wallet (KeyWallet)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey(PrivatePaymentKey))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left), either, isLeft)
import Data.Foldable (fold, sum)
import Data.HTTP.Method as Method
import Data.Lens ((^.))
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.String (joinWith)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (foldMap, for, for_, sequence_, traverse, traverse_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (Aff, Milliseconds(Milliseconds), delay, try)
import Effect.Aff (bracket) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mote (bracket) as Mote
import Mote.Description (Description(Group, Test))
import Mote.Monad (MoteT(MoteT), mapTest)
import Node.ChildProcess (defaultSpawnOptions)
import Node.FS.Aff (mkdir)
import Node.FS.Sync (exists, mkdir) as FSSync
import Node.Path (FilePath, dirname)
import Node.Path (concat) as Path
import Type.Prelude (Proxy(Proxy))

-- | Run a single `Contract` in Plutip environment.
runPlutipContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (wallets -> Contract a)
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
  -> (ContractEnv -> wallets -> Aff a)
  -> Aff a
withPlutipContractEnv plutipCfg distr cont = do
  cleanupRef <- liftEffect $ Ref.new mempty
  Aff.bracket
    (try $ startPlutipContractEnv plutipCfg distr cleanupRef)
    (const $ runCleanup cleanupRef)
    $ liftEither >=> \{ env, wallets, printLogs } ->
        whenError printLogs (cont env wallets)

-- | Run `PlutipTest`s with an existing `ContractEnv`, not necessarily one
-- | created through `Plutip`.
-- | Tests are funded by the wallet in the supplied environment.
-- | The `FilePath` parameter should point to a directory to store generated
-- | wallets, in the case where funds failed to be returned to the main wallet.
testContractsInEnv
  :: ContractParams
  -> FilePath
  -> TestPlanM PlutipTest Unit
  -> TestPlanM (Aff Unit) Unit
testContractsInEnv params backup = mapTest \(PlutipTest runPlutipTest) ->
  runPlutipTest \distr mkTest -> withContractEnv params \env -> do
    let
      distrArray :: Array (Array UtxoAmount)
      distrArray = encodeDistribution distr

    privateKeys <- liftEffect $ for distrArray \_ -> freshPrivateKey <#>
      PrivateKeyResponse

    wallets <-
      liftMaybe
        ( error
            "Impossible happened: could not decode wallets. Please report as bug"
        )
        $ decodeWallets distr privateKeys

    let
      walletsArray :: Array KeyWallet
      walletsArray = keyWallets (pureProxy distr) wallets

      runContract :: Aff Unit
      runContract = runContractInEnv env { wallet = Nothing } do
        logTrace' "Running contract"
        mkTest wallets

    if Array.null walletsArray then
      runContract
    else Aff.bracket
      ( backupWallets env walletsArray *> fundWallets env walletsArray
          distrArray
      )
      -- Retry fund returning until success or timeout. Submission will fail if
      -- the node has seen the wallets utxos being spent previously, so retrying
      -- will allow the wallets utxos to eventually represent a spendable set
      ( \funds -> recovering returnFundsRetryPolicy ([ \_ _ -> pure true ])
          \_ -> returnFunds env walletsArray funds
      )
      \_ -> runContract
  where
  pureProxy :: forall (a :: Type). a -> Proxy a
  pureProxy _ = Proxy

  backupWallets :: ContractEnv -> Array KeyWallet -> Aff Unit
  backupWallets env walletsArray = liftAff $ for_ walletsArray \wallet -> do
    let
      address = addressBech32 $ (unwrap wallet).address env.networkId
      payment = keyWalletPrivatePaymentKey wallet
      mbStake = keyWalletPrivateStakeKey wallet
      folder = Path.concat [ backup, address ]

    mkdir folder
    privatePaymentKeyToFile (Path.concat [ folder, "payment_signing_key" ])
      payment
    for mbStake $ privateStakeKeyToFile
      (Path.concat [ folder, "stake_signing_key" ])

  fundWallets
    :: ContractEnv -> Array KeyWallet -> Array (Array UtxoAmount) -> Aff BigInt
  fundWallets env walletsArray distrArray = runContractInEnv env do
    logTrace' "Funding wallets"
    let
      constraints = flip foldMap (Array.zip walletsArray distrArray)
        \(wallet /\ walletDistr) -> flip foldMap walletDistr
          \value -> mustPayToKeyWallet wallet $ lovelaceValueOf value

    txHash <- submitTxFromConstraints (mempty :: _ Void) constraints
    awaitTxConfirmed txHash
    let fundTotal = Array.foldr (+) zero $ join distrArray
    -- Use log so we can see, regardless of suppression
    log $ joinWith " "
      [ "Sent"
      , BigInt.toString fundTotal
      , "lovelace to test wallets"
      ]
    pure fundTotal

  returnFundsRetryPolicy :: RetryPolicy
  returnFundsRetryPolicy = limitRetriesByCumulativeDelay
    (Milliseconds 30_000.00)
    (constantDelay $ Milliseconds 2_000.0)

  returnFunds :: ContractEnv -> Array KeyWallet -> BigInt -> Aff Unit
  returnFunds env walletsArray fundTotal = runContractInEnv env do
    logTrace' "Returning wallet funds"

    utxos <- Map.unions <<< fold <$> for walletsArray
      (flip withKeyWallet getWalletAddresses >=> traverse utxosAt)

    pkhs <- fold <$> for walletsArray
      (flip withKeyWallet ownPaymentPubKeysHashes)

    let
      constraints = flip foldMap (Map.keys utxos) mustSpendPubKeyOutput
        <> foldMap mustBeSignedBy pkhs
      lookups = unspentOutputs utxos

    unbalancedTx <- liftedE $ mkUnbalancedTx (lookups :: _ Void) constraints
    balancedTx <- liftedE $ balanceTx unbalancedTx
    balancedSignedTx <- Array.foldM
      (\tx wallet -> withKeyWallet wallet $ signTransaction tx)
      (wrap $ unwrap balancedTx)
      walletsArray

    txHash <- submit balancedSignedTx
    awaitTxConfirmed txHash

    let
      (refundTotal :: BigInt) = Array.foldr
        (\txorf acc -> acc + valueToCoin' (txorf ^. _output ^. _amount))
        zero
        (Array.fromFoldable $ Map.values utxos)

    log $ joinWith " "
      [ "Refunded"
      , BigInt.toString refundTotal
      , "of"
      , BigInt.toString fundTotal
      , "lovelace from test wallets"
      ]

  mustPayToKeyWallet
    :: forall (i :: Type) (o :: Type)
     . KeyWallet
    -> Value
    -> TxConstraints i o
  mustPayToKeyWallet wallet value =
    let
      convert = wrap <<< publicKeyHash <<< publicKeyFromPrivateKey
      payment = over wrap convert $ keyWalletPrivatePaymentKey wallet
      mbStake = over wrap convert <$> keyWalletPrivateStakeKey wallet
    in
      maybe
        (mustPayToPubKey payment)
        (mustPayToPubKeyAddress payment)
        mbStake
        value

-- | Run `Contract`s in tests in a single Plutip instance.
-- | NOTE: This uses `MoteT`s bracketting, and thus has the same caveats.
-- |       Namely, brackets are run for each of the following groups and tests.
-- |       If you wish to only set up Plutip once, ensure all tests are wrapped
-- |       in a single group.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md#testing-with-mote
testPlutipContracts
  :: PlutipConfig
  -> TestPlanM PlutipTest Unit
  -> TestPlanM (Aff Unit) Unit
testPlutipContracts plutipCfg tp = do
  PlutipTestPlan runPlutipTestPlan <- lift $ execDistribution tp
  runPlutipTestPlan \distr tests -> do
    cleanupRef <- liftEffect $ Ref.new mempty
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

runCleanup :: Ref (Array (Aff Unit)) -> Aff Unit
runCleanup cleanupRef = do
  cleanups <- liftEffect $ Ref.read cleanupRef
  sequence_ (try <$> cleanups)

-- Similar to `catchError` but preserves the error
whenError :: forall (a :: Type). Aff Unit -> Aff a -> Aff a
whenError whenErrorAction action = do
  res <- try action
  when (isLeft res) whenErrorAction
  liftEither res

-- | Represents `Contract`s that depend on *some* wallet `UtxoDistribution`
newtype PlutipTest = PlutipTest
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . PlutipTestHandler distr wallets r
       )
    -> r
  )

type PlutipTestHandler :: Type -> Type -> Type -> Type
type PlutipTestHandler distr wallets r =
  UtxoDistribution distr wallets => distr -> (wallets -> Contract Unit) -> r

-- | Store a wallet `UtxoDistribution` and a `Contract` that depends on those wallets
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract Unit)
  -> PlutipTest
withWallets distr tests = PlutipTest \h -> h distr tests

-- | Lift a `Contract` into `PlutipTest`
noWallet :: Contract Unit -> PlutipTest
noWallet = withWallets unit <<< const

-- | Represents `Contract`s in `TestPlanM` that depend on *some* wallet `UtxoDistribution`
newtype PlutipTestPlan = PlutipTestPlan
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . PlutipTestPlanHandler distr wallets r
       )
    -> r
  )

type PlutipTestPlanHandler :: Type -> Type -> Type -> Type
type PlutipTestPlanHandler distr wallets r =
  UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> r

-- | Lifts the utxo distributions of each test out of Mote, into a combined
-- | distribution. Adapts the tests to pick their distribution out of the
-- | combined distribution.
-- | NOTE: Skipped tests still have their distribution generated.
execDistribution :: TestPlanM PlutipTest Unit -> Aff PlutipTestPlan
execDistribution (MoteT mote) = execWriterT mote <#> go
  where
  go :: Array (Description Aff PlutipTest) -> PlutipTestPlan
  go = flip execState emptyPlutipTestPlan <<< traverse_ case _ of
    Test rm { bracket, label, value: PlutipTest runPlutipTest } ->
      runPlutipTest \distr test -> do
        addTests distr $ MoteT
          (tell [ Test rm { bracket, label, value: test } ])
    Group rm { bracket, label, value } -> do
      let PlutipTestPlan runGroupPlan = go value
      runGroupPlan \distr tests ->
        addTests distr $ over MoteT
          (censor (pure <<< Group rm <<< { bracket, label, value: _ }))
          tests

  addTests
    :: forall (distr :: Type) (wallets :: Type)
     . PlutipTestPlanHandler distr wallets (State PlutipTestPlan Unit)
  addTests distr tests = do
    modify_ \(PlutipTestPlan runPlutipTestPlan) -> runPlutipTestPlan
      \distr' tests' -> PlutipTestPlan \h -> h (distr' /\ distr) do
        mapTest (_ <<< fst) tests'
        mapTest (_ <<< snd) tests

  emptyPlutipTestPlan :: PlutipTestPlan
  emptyPlutipTestPlan = PlutipTestPlan \h -> h unit (pure unit)

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
  startPlutipServer'
  ourKey /\ response <- startPlutipCluster'
  startOgmios' response
  startKupo' response
  { env, printLogs, clearLogs } <- mkContractEnv'
  wallets <- mkWallets' env ourKey response
  pure
    { env
    , wallets
    , printLogs
    , clearLogs
    }
  where
  -- Similar to `Aff.bracket`, except cleanup is pushed onto a stack to be run
  -- later.
  bracket
    :: forall (a :: Type) (b :: Type)
     . Aff a
    -> (a -> Aff Unit)
    -> (a -> Aff b)
    -> Aff b
  bracket before after action = do
    Aff.bracket
      before
      (\res -> liftEffect $ Ref.modify_ ([ after res ] <> _) cleanupRef)
      action

  startPlutipServer' :: Aff Unit
  startPlutipServer' =
    bracket (startPlutipServer plutipCfg)
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
    for_ distrArray $ traverse_ \n -> when (n < BigInt.fromInt 1_000_000) do
      liftEffect $ throw $ "UTxO is too low: " <> BigInt.toString n <>
        ", must be at least 1_000_000 Lovelace"
    bracket
      (startPlutipCluster plutipCfg distrArray)
      (const $ void $ stopPlutipCluster plutipCfg)
      pure

  startOgmios' :: ClusterStartupParameters -> Aff Unit
  startOgmios' response =
    bracket (startOgmios plutipCfg response)
      (stopChildProcessWithPort plutipCfg.ogmiosConfig.port)
      (const $ pure unit)

  startKupo' :: ClusterStartupParameters -> Aff Unit
  startKupo' response =
    bracket (startKupo plutipCfg response)
      (stopChildProcessWithPortAndRemoveOnSignal plutipCfg.kupoConfig.port)
      \(process /\ workdir /\ _) -> do
        liftEffect $ cleanupTmpDir process workdir
        pure unit

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
            $ decodeWallets distr response.privateKeys
        let walletsArray = keyWallets (Proxy :: Proxy distr) wallets
        transferFundsFromEnterpriseToBase ourKey walletsArray
        pure wallets

  mkContractEnv'
    :: Aff
         { env :: ContractEnv
         , printLogs :: Aff Unit
         , clearLogs :: Aff Unit
         }
  mkContractEnv' | plutipCfg.suppressLogs = do
    -- if logs should be suppressed, setup the machinery and continue with
    -- the bracket
    { addLogEntry, suppressedLogger, printLogs, clearLogs } <-
      liftEffect $ setupLogs plutipCfg.logLevel plutipCfg.customLogger
    let
      configLogger = Just $ map liftEffect <<< addLogEntry

    bracket (mkClusterContractEnv plutipCfg suppressedLogger configLogger)
      stopContractEnv
      \env -> pure
        { env
        , printLogs: liftEffect printLogs
        , clearLogs: liftEffect clearLogs
        }
  mkContractEnv' =
    -- otherwise, proceed with the env setup and provide a normal logger
    bracket
      ( mkClusterContractEnv plutipCfg
          (mkLogger plutipCfg.logLevel plutipCfg.customLogger)
          plutipCfg.customLogger
      )
      stopContractEnv
      \env -> pure
        { env
        , printLogs: pure unit
        , clearLogs: pure unit
        }

-- | Throw an exception if `PlutipConfig` contains ports that are occupied.
configCheck :: PlutipConfig -> Aff Unit
configCheck cfg = do
  let
    services :: Array (UInt /\ String)
    services =
      [ cfg.port /\ "plutip-server"
      , cfg.ogmiosConfig.port /\ "ogmios"
      , cfg.kupoConfig.port /\ "kupo"
      ]
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
  let
    url = mkServerEndpointUrl cfg "start"
    -- TODO epoch size cannot currently be changed due to
    -- https://github.com/mlabs-haskell/plutip/issues/149
    epochSize = UInt.fromInt 80
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
                    , slotLength: cfg.clusterConfig.slotLength
                    , epochSize
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
      \{ body } -> lmap (ClientDecodeJsonError body)
        $ (decodeAeson <=< parseJsonStringToAeson)
            body
  either (liftEffect <<< throw <<< show) pure res

startOgmios :: PlutipConfig -> ClusterStartupParameters -> Aff ManagedProcess
startOgmios cfg params = do
  spawn "ogmios" ogmiosArgs defaultSpawnOptions
    $ Just
    $ String.indexOf (Pattern "networkParameters")
        >>> maybe NoOp (const Success)
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

startKupo
  :: PlutipConfig
  -> ClusterStartupParameters
  -> Aff (ManagedProcess /\ String /\ OnSignalRef)
startKupo cfg params = do
  tmpDir <- liftEffect tmpdir
  let
    workdir = tmpDir <</>> "kupo-db"
    testClusterDir = (dirname <<< dirname) params.nodeConfigPath
  liftEffect do
    workdirExists <- FSSync.exists workdir
    unless workdirExists (FSSync.mkdir workdir)
  childProcess <- spawnKupoProcess workdir
  sig <- liftEffect $ cleanupOnSigint workdir testClusterDir
  pure (childProcess /\ workdir /\ sig)
  where
  spawnKupoProcess :: FilePath -> Aff ManagedProcess
  spawnKupoProcess workdir =
    spawn "kupo" (kupoArgs workdir) defaultSpawnOptions $
      Just (String.indexOf outputString >>> maybe NoOp (const Success))
    where
    outputString :: Pattern
    outputString = Pattern "ConfigurationCheckpointsForIntersection"

  kupoArgs :: FilePath -> Array String
  kupoArgs workdir =
    [ "--match"
    , "*/*"
    , "--since"
    , "origin"
    , "--workdir"
    , workdir
    , "--host"
    , cfg.kupoConfig.host
    , "--port"
    , UInt.toString cfg.kupoConfig.port
    , "--node-socket"
    , params.nodeSocketPath
    , "--node-config"
    , params.nodeConfigPath
    ]

startPlutipServer :: PlutipConfig -> Aff ManagedProcess
startPlutipServer cfg = do
  spawn "plutip-server" [ "-p", UInt.toString cfg.port ]
    defaultSpawnOptions
    Nothing

checkPlutipServer :: PlutipConfig -> Aff Unit
checkPlutipServer cfg = do
  -- We are trying to call stopPlutipCluster endpoint to ensure that
  -- `plutip-server` has started.
  void
    $ recovering defaultRetryPolicy
        ([ \_ _ -> pure true ])
    $ const
    $ stopPlutipCluster cfg

-- | Kill a process and wait for it to stop listening on a specific port.
stopChildProcessWithPort :: UInt -> ManagedProcess -> Aff Unit
stopChildProcessWithPort port childProcess = do
  stop childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

stopChildProcessWithPortAndRemoveOnSignal
  :: UInt -> (ManagedProcess /\ String /\ OnSignalRef) -> Aff Unit
stopChildProcessWithPortAndRemoveOnSignal port (childProcess /\ _ /\ sig) = do
  stop $ childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"
  liftEffect $ removeOnSignal sig

mkClusterContractEnv
  :: PlutipConfig
  -> Logger
  -> Maybe (LogLevel -> Message -> Aff Unit)
  -> Aff ContractEnv
mkClusterContractEnv plutipCfg logger customLogger = do
  usedTxOuts <- newUsedTxOuts
  backend <- buildBackend logger $ mkCtlBackendParams
    { ogmiosConfig: plutipCfg.ogmiosConfig
    , kupoConfig: plutipCfg.kupoConfig
    }
  ledgerConstants <- getLedgerConstants
    plutipCfg { customLogger = customLogger }
    backend
  pure
    { backend
    , networkId: MainnetId
    , logLevel: plutipCfg.logLevel
    , walletSpec: Nothing
    , customLogger: customLogger
    , suppressLogs: plutipCfg.suppressLogs
    , hooks: emptyHooks
    , wallet: Nothing
    , usedTxOuts
    , ledgerConstants
    }

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

mkServerEndpointUrl :: PlutipConfig -> String -> String
mkServerEndpointUrl cfg path = do
  "http://" <> cfg.host <> ":" <> UInt.toString cfg.port <</>> path
