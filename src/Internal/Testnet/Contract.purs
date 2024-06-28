module Ctl.Internal.Testnet.Contract
  ( runTestnetContract
  , runTestnetTestPlan
  , testTestnetContracts
  , withTestnetContractEnv
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib (privateKey_generateEd25519) as Csl
import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types.Address (Address, getPaymentCredential, getStakeCredential)
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.PaymentCredential (PaymentCredential(PaymentCredential))
import Cardano.Types.StakeCredential (StakeCredential(StakeCredential))
import Cardano.Types.StakePubKeyHash (StakePubKeyHash(StakePubKeyHash))
import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , ContractEnv
  , liftContractM
  , liftedM
  , runContractInEnv
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet
  ( getWalletAddress
  , mkKeyWalletFromPrivateKeys
  , withKeyWallet
  )
import Control.Monad.State (State, execState, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (censor, execWriterT, tell)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddress)
import Ctl.Internal.Plutip.Server (makeClusterContractEnv)
import Ctl.Internal.Plutip.Utils (cleanupOnExit, runCleanup, whenError)
import Ctl.Internal.Test.ContractTest
  ( ContractTest(ContractTest)
  , ContractTestPlan(ContractTestPlan)
  , ContractTestPlanHandler
  )
import Ctl.Internal.Test.UtxoDistribution
  ( class UtxoDistribution
  , UtxoAmount
  , decodeWallets
  , encodeDistribution
  , keyWallets
  )
import Ctl.Internal.Testnet.Server (StartedTestnetCluster, startTestnetCluster)
import Ctl.Internal.Testnet.Types (TestnetConfig)
import Ctl.Internal.Testnet.Utils (read872GenesisKey)
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Data.Array (head, zip) as Array
import Data.Maybe (fromJust)
import Effect.Aff (bracket) as Aff
import Effect.Aff (try)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Internal.CardanoCli.QueryHandler (withCardanoCliCompletion)
import Mote (bracket) as Mote
import Mote.Description (Description(Group, Test))
import Mote.Monad (MoteT(MoteT), mapTest)
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))

-- | Run a single `Contract` in cardano-testnet environment.
runTestnetContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => TestnetConfig
  -> distr
  -> (wallets -> Contract a)
  -> Aff a
runTestnetContract cfg distr cont =
  withTestnetContractEnv cfg distr \env wallets ->
    runContractInEnv env (cont wallets)

-- | Provide a `ContractEnv` connected to cardano-testnet.
-- | Can be used to run multiple `Contract`s using `runContractInEnv`.
withTestnetContractEnv
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => TestnetConfig
  -> distr
  -> (ContractEnv -> wallets -> Aff a)
  -> Aff a
withTestnetContractEnv cfg distr cont = do
  cleanupRef <- liftEffect $ Ref.new mempty
  Aff.bracket
    (try $ startTestnetContractEnv cfg distr cleanupRef)
    (const $ runCleanup cleanupRef)
    $ liftEither
    >=> \{ env, wallets, printLogs } ->
      whenError printLogs (cont env wallets)

-- | Run several `Contract`s in tests in a (single) cardano-testnet environment (cardano-testnet, ogmios, kupo, etc.).
-- | NOTE: This uses `MoteT`s bracketing, and thus has the same caveats.
-- |       Namely, brackets are run for each of the top-level groups and tests
-- |       inside the bracket.
-- |       If you wish to only set up Plutip once, ensure all tests that are passed
-- |       to `testPlutipContracts` are wrapped in a single group.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md#testing-with-mote FIXME
testTestnetContracts
  :: TestnetConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
testTestnetContracts cfg tp = do
  testnetTestPlan <- lift $ execDistribution tp
  runTestnetTestPlan cfg testnetTestPlan

-- | Run a `ContractTestPlan` in a (single) cardano-testnet environment.
-- | Supports wallet reuse - see docs on sharing wallet state between
-- | wallets in `doc/plutip-testing.md`. FIXME
runTestnetTestPlan
  :: TestnetConfig
  -> ContractTestPlan
  -> TestPlanM (Aff Unit) Unit
runTestnetTestPlan cfg (ContractTestPlan runContractTestPlan) = do
  -- Modify tests to pluck out parts of a single combined distribution
  runContractTestPlan \distr tests -> do
    cleanupRef <- liftEffect $ Ref.new mempty
    -- Sets a single Mote bracket at the top level, it will be run for all
    -- immediate tests and groups
    bracket (startTestnetContractEnv cfg distr cleanupRef)
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
    resultRef <- liftEffect $ Ref.new
      (Left $ error "cardano-testnet not initialized")
    let
      before = do
        res <- try $ before'
        liftEffect $ Ref.write res resultRef
        pure res
      after = const $ after'
    Mote.bracket { before, after } $ flip mapTest act \t -> do
      result <- liftEffect $ Ref.read resultRef >>= liftEither
      t result

-- | Lifts the UTxO distributions of each test out of Mote, into a combined
-- | distribution. Adapts the tests to pick their distribution out of the
-- | combined distribution.
-- | NOTE: Skipped tests still have their distribution generated.
-- | This is the current method of constructing all the wallets with required distributions
-- | in one go during Plutip startup.
execDistribution :: TestPlanM ContractTest Unit -> Aff ContractTestPlan
execDistribution (MoteT mote) = execWriterT mote <#> go
  where
  -- Recursively go over the tree of test `Description`s and construct a `ContractTestPlan` callback.
  -- When run the `ContractTestPlan` will reconstruct the whole `MoteT` value passed to `execDistribution`
  -- via similar writer effects (plus combining distributions) which append test descriptions
  -- or wrap them in a group.
  go :: Array (Description Aff ContractTest) -> ContractTestPlan
  go = flip execState emptyContractTestPlan <<< traverse_ case _ of
    Test rm { bracket, label, value: ContractTest runTest } ->
      runTest \distr test -> do
        addTests distr $ MoteT
          (tell [ Test rm { bracket, label, value: test } ])
    Group rm { bracket, label, value } -> do
      let ContractTestPlan runGroupPlan = go value
      runGroupPlan \distr tests ->
        addTests distr $ over MoteT
          (censor (pure <<< Group rm <<< { bracket, label, value: _ }))
          tests

  -- This function is used by `go` for iteratively adding Mote tests (internally Writer monad actions)
  -- to the `ContractTestPlan` in the State monad _and_ for combining UTxO distributions used by tests.
  -- Given a distribution and tests (a MoteT value) this runs a `ContractTestPlan`, i.e. passes its
  -- stored distribution and tests to our handler, and then makes a new `ContractTestPlan`, but this time
  -- storing a tuple of stored and passed distributions and also storing a pair of Mote tests, modifying
  -- the previously stored tests to use the first distribution, and the passed tests the second distribution
  --
  -- `go` starts at the top of the test tree and step-by-step constructs a big `ContractTestPlan` which
  -- stores distributions of all inner tests tupled together and tests from the original test tree, which
  -- know how to get their distribution out of the big tuple.
  addTests
    :: forall (distr :: Type) (wallets :: Type)
     . ContractTestPlanHandler distr wallets (State ContractTestPlan Unit)
  addTests distr tests = do
    modify_ \(ContractTestPlan runContractTestPlan) -> runContractTestPlan
      \distr' tests' -> ContractTestPlan \h -> h (distr' /\ distr) do
        mapTest (_ <<< fst) tests'
        mapTest (_ <<< snd) tests

  -- Start with an empty plan, which passes an empty distribution
  -- and an empty array of test `Description`s to the function that
  -- will run tests.
  emptyContractTestPlan :: ContractTestPlan
  emptyContractTestPlan = ContractTestPlan \h -> h unit (pure unit)

-- | Provide a `ContractEnv` connected to cardano-testnet.
-- | can be used to run multiple `Contract`s using `runContractInEnv`.
startTestnetContractEnv
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => TestnetConfig
  -> distr
  -> Ref (Array (Aff Unit))
  -> Aff
       { cluster :: StartedTestnetCluster
       , env :: ContractEnv
       , wallets :: wallets
       , printLogs :: Aff Unit
       , clearLogs :: Aff Unit
       }
startTestnetContractEnv cfg distr cleanupRef = do
  _ <- cleanupOnExit cleanupRef
  cluster <- startTestnetCluster cfg cleanupRef
  { env, printLogs, clearLogs } <- makeClusterContractEnv cleanupRef cfg
  let env' = env { networkId = TestnetId }
  wallets <- mkWallets env' cluster
  pure
    { cluster
    , env: env'
    , wallets
    , printLogs
    , clearLogs
    }
  where
  mkWallets :: ContractEnv -> StartedTestnetCluster -> Aff wallets
  mkWallets env cluster =
    runContractInEnv env do
      genesisWallets <- liftEffect readGenesisWallets
      let
        genesisWallet = unsafePartial fromJust $ Array.head genesisWallets -- FIXME
      withKeyWallet genesisWallet do
        let distrArray = encodeDistribution distr
        privateKeys <-
          for (encodeDistribution distr) \_ ->
            liftEffect $ wrap <$> Csl.privateKey_generateEd25519
        wallets <-
          liftContractM
            "Impossible happened: could not decode wallets. Please report as bug"
            $ decodeWallets distr privateKeys
        let kws = keyWallets (Proxy :: _ distr) wallets
        genesisAddr <- liftedM "Could not get genesis address" getWalletAddress
        let
          nodeCfg =
            { socketPath: (unwrap cluster).paths.nodeSocketPath
            , testnetMagic: cfg.clusterConfig.testnetMagic
            }
        withCardanoCliCompletion nodeCfg genesisAddr do
          let walletsAmounts = Array.zip kws distrArray
          fundWalletsFromGenesis walletsAmounts
        pure wallets
    where
    readGenesisWallets :: Effect (Array KeyWallet)
    readGenesisWallets =
      traverse
        ( \location -> do
            paymentKey <- read872GenesisKey location
            pure $ mkKeyWalletFromPrivateKeys paymentKey Nothing
        )
        (unwrap cluster).paths.genesisKeys

  fundWalletsFromGenesis
    :: Array (KeyWallet /\ Array UtxoAmount) -> Contract Unit
  fundWalletsFromGenesis walletsAmounts = do
    network <- getNetworkId
    let
      constraints :: TxConstraints
      constraints =
        foldMap
          ( \(KeyWallet kw /\ amounts) ->
              foldMap
                ( mustPayToAddress (kw.address network) <<<
                    Value.lovelaceValueOf
                )
                amounts
          )
          walletsAmounts
    unbalancedTx <- mkUnbalancedTx mempty constraints
    balancedTx <- balanceTx unbalancedTx
    balancedSignedTx <- signTransaction balancedTx
    txHash <- submit balancedSignedTx
    logInfo' $ "FundWalletsFromGenesis txHash: " <> show txHash
    awaitTxConfirmed txHash

  mustPayToAddress :: Address -> Value -> TxConstraints
  mustPayToAddress addr =
    let
      skh = case getStakeCredential addr of
        Just (StakeCredential (PubKeyHashCredential skh')) -> Just $
          StakePubKeyHash skh'
        _ -> Nothing
    in
      case getPaymentCredential addr of
        Just (PaymentCredential (PubKeyHashCredential pkh)) ->
          mustPayToPubKeyStakeAddress (wrap pkh) skh
        _ -> mempty
