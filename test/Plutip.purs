module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types as Cardano.Types
import Cardano.Types.Address (Address, getPaymentCredential, getStakeCredential)
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.BigInt (BigInt)
import Cardano.Types.BigNum (BigNum(..))
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Credential (Credential(..))
import Cardano.Types.PaymentCredential (PaymentCredential(..))
import Cardano.Types.PrivateKey as Cardano.Type.PrivateKey
import Cardano.Types.PublicKey as Cardano.Types.PublicKey
import Cardano.Types.StakeCredential (StakeCredential(..))
import Cardano.Types.StakePubKeyHash (StakePubKeyHash(..))
import Contract.Address (getNetworkId)
import Contract.Monad (Contract(..), liftedM)
import Contract.Monad as Contract
import Contract.Test.Plutip (defaultPlutipConfig)
import Contract.Test.Utils (interruptOnSignal)
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (DatumPresence(..), TxConstraints(..))
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (Value(..), lovelaceValueOf, valueToCoin)
import Contract.Wallet (getWalletAddress, getWalletBalance, mkKeyWalletFromPrivateKeys, withKeyWallet)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddress)
import Ctl.Examples.OneShotMinting (contract) as OneShotMinting
import Ctl.Examples.PlutusV2.ReferenceInputs (contract) as ReferenceInputs
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Testnet.Contract (readGenesisWallets)
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types (CardanoTestnetStartupParams)
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Types.TxConstraints (TxConstraint(..))
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Data.Array (head, singleton)
import Data.Maybe (Maybe(Just), fromJust)
import Data.Posix.Signal (Signal(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff)
import Internal.CardanoCli.QueryHandler as CardanoCli.QueryHandler
import Partial.Unsafe (unsafePartial)
import Record as Record

defaultStartupParams :: { | CardanoTestnetStartupParams () }
defaultStartupParams =
  ( Testnet.Types.defaultStartupParams
      { testnetMagic: 2 }
  )
    { nodeLoggingFormat = Just Testnet.Types.LogAsJson }

newWallet :: Effect KeyWallet
newWallet = ado
  paymentSkey <- wrap <$> genSkey
  stakingSkey <- wrap <$> genSkey
  in mkKeyWalletFromPrivateKeys paymentSkey (Just stakingSkey)
  where
  genSkey = wrap <$> CSL.privateKey_generateEd25519

walletInfo
  :: Cardano.Types.NetworkId
  -> KeyWallet
  -> { address :: Cardano.Types.Address
     , paymentPubKeyHash :: Cardano.Types.PaymentPubKeyHash
     , stakePubKeyHash :: Maybe Cardano.Types.StakePubKeyHash
     }
walletInfo network (KeyWallet wallet) =
  let
    privateKeyToPkh =
      Cardano.Types.PublicKey.hash
        <<< Cardano.Type.PrivateKey.toPublicKey

    paymentPubKeyHash :: Cardano.Types.PaymentPubKeyHash
    paymentPubKeyHash =
      wrap $ privateKeyToPkh $ unwrap wallet.paymentKey

    stakePubKeyHash :: Maybe Cardano.Types.StakePubKeyHash
    stakePubKeyHash =
      wrap <<< privateKeyToPkh <<< unwrap <$> wallet.stakeKey
    address =
      Cardano.Types.Address.mkPaymentAddress
        network
        (wrap $ Cardano.Types.PubKeyHashCredential $ unwrap paymentPubKeyHash)
        $ wrap
        <<< Cardano.Types.PubKeyHashCredential
        <<< unwrap
        <$> stakePubKeyHash
  in
    { paymentPubKeyHash
    , stakePubKeyHash
    , address
    }

-- Run with `npm run plutip-test`
main :: Effect Unit
main = (interruptOnSignal SIGINT =<< _) $ launchAff $ void do
  let cfg = Record.union defaultStartupParams defaultPlutipConfig
      distr = [ BigNum.fromInt 50_000_000, BigNum.fromInt 5_000_000 ]
  Testnet.Contract.withTestnetContractEnv cfg distr \contractEnv wallet -> do
    log "Running test contract"
    Contract.runContractInEnv contractEnv do
      withKeyWallet wallet do
        balance <- getWalletBalance
        traceM $ "kw balance: " <> show balance
        OneShotMinting.contract

{-
      log "Inside"
      let
        Testnet.MkStartedTestnetCluster { paths } = cluster
      -- a new wallet to send initial lovelace to
      -- do something interesting with every utxo in utxo-keys
      genesisWallets <- liftEffect $ readGenesisWallets paths
      let genesisWallet = unsafePartial fromJust $ head genesisWallets
      {-
      for_ genesisWallets \wallet -> do
        withKeyWallet wallet do
          let
            nodeCfg =
              { socketPath: paths.nodeSocketPath
              , testnetMagic: cfg.testnetMagic
              }
          -- it will show zero balance
          log
            <<< show
            <<< { initialBalanceViaKupo: _ }
            =<< getWalletBalance

          -- we can see genesis utxos when add cardano-cli query results to utxosAt
          CardanoCli.QueryHandler.withCardanoCliCompletion nodeCfg do
            log <<< show <<< { utxosAfterTx: _ } <<< map valueToCoin =<<
              getWalletBalance -- utxosAt newWallet.address
      kw <- withKeyWallet genesisWallet do
        let
          nodeCfg =
            { socketPath: paths.nodeSocketPath
            , testnetMagic: cfg.testnetMagic
            }
        -- it will show zero balance
        log
          <<< show
          <<< { initialBalanceViaKupo: _ }
          =<< getWalletBalance

        genesisAddr <- liftedM "Could not get genesis address" getWalletAddress

        -- we can see genesis utxos when add cardano-cli query results to utxosAt
        CardanoCli.QueryHandler.withCardanoCliCompletion nodeCfg genesisAddr do
          -- log <<< show <<< { utxosAfterTx: _ } <<< map valueToCoin =<<
          -- getWalletBalance -- utxosAt newWallet.address

          -- send 100 ADA to a newly generated wallet
          wallet <- liftEffect newWallet
          fundWalletFromGenesis wallet genesisWallet (BigNum.fromInt 100_000_000)
          fundWalletFromGenesis wallet genesisWallet (BigNum.fromInt 5_000_000)
          pure wallet
      -- execute simple contract
      withKeyWallet kw do
        kwBalance <- getWalletBalance
        traceM $ "kw balance: " <> show kwBalance
        OneShotMinting.contract

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir
-}

-- flip cancelWith (effectCanceler (exitCode 1)) do
--   Utils.interpretWithConfig
--     defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
--     $ group "Plutip" do
--         testTestnetContracts config Mnemonics.suite
--         group "ExUnits - normal limits" do
--           testTestnetContracts config $ ExUnits.mkFailingSuite 3000
--           testTestnetContracts config $ ExUnits.mkSuite 2550
--         group "ExUnits - relaxed limits" do
--           testTestnetContracts configWithMaxExUnits $ ExUnits.mkSuite 3000
--         testTestnetContracts config Assert.suite
--         Logging.suite
--         testStartTestnet
--         testTestnetContracts config $ do
--           flip mapTest QueryM.AffInterface.suite
--             (noWallet <<< wrapQueryM)
--           ChangeGeneration.suite
--           Contract.suite
--         UtxoDistribution.suite
--         testTestnetContracts config OgmiosMempool.suite
--         runTestnetTestPlan config SameWallets.suite
--         ClusterParameters.runTest

-- configWithMaxExUnits :: PlutipConfig
-- configWithMaxExUnits = config
--   { clusterConfig = config.clusterConfig { raiseExUnitsToMax = true } }

-- testStartTestnet:: TestPlanM (Aff Unit) Unit
-- testStartTestnet = group "Server" do
--   test "startTestnet / stopTestnet" do
--     -- bracket (startTestnet config)
--     --   (stopChildProcessWithPort config.port) $ const do
--       checkTestnet config
--       _startRes <- startTestnet config [ [] ]
--       stopRes <- stopTestnet config
--       stopRes `shouldSatisfy` case _ of
--         StopClusterSuccess -> true
--         _ -> false
