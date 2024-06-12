module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.PrivateKey as Cardano.Type.PrivateKey
import Cardano.Types.PublicKey as Cardano.Types.PublicKey
import Contract.Monad as Contract
import Contract.Test.Plutip (defaultPlutipConfig)
import Contract.Test.Utils
  ( interruptOnSignal
  )
import Contract.Value
  ( valueToCoin
  )
import Contract.Wallet
  ( getWalletBalance
  , mkKeyWalletFromPrivateKeys
  , withKeyWallet
  )
import Ctl.Internal.Plutip.Utils
  ( tmpdir
  )
import Ctl.Internal.Testnet.Contract
  ( readGenesisWallets
  )
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Internal.CardanoCli.QueryHandler as CardanoCli.QueryHandler
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
  _ <- Testnet.Contract.withContractEnv cfg \cluster env -> do
    log "Running test contract"
    Contract.runContractInEnv env do
      log "Inside"
      let
        Testnet.MkStartedTestnetCluster { paths } = cluster
      -- a new wallet to send initial lovelace to
      -- do something interesting with every utxo in utxo-keys
      genesisWallets <- liftEffect $ readGenesisWallets paths
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

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir

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

