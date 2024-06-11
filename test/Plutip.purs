module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types as Cardano.Types
import Cardano.Types.Address as Cardano.Types.Address
import Cardano.Types.PrivateKey as Cardano.Type.PrivateKey
import Cardano.Types.PublicKey as Cardano.Types.PublicKey
import Contract.ClientError as Contract.ClientError
import Contract.Config as Config
import Contract.Monad (ContractEnv)
import Contract.Monad (runContractInEnv) as Contract
import Contract.Test.Plutip (defaultPlutipConfig)
import Contract.Test.Utils
  ( interruptOnSignal
  )
import Contract.TextEnvelope
  ( TextEnvelope(..)
  , TextEnvelopeType(..)
  , decodeTextEnvelope
  )
import Contract.Value
  ( valueToCoin
  )
import Contract.Wallet
  ( getWalletBalance
  , mkKeyWalletFromPrivateKeys
  , withKeyWallet
  )
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromTextEnvelope
  )
import Control.Monad.Error.Class
  ( liftMaybe
  , try
  )
import Control.Monad.Except
  ( ExceptT(..)
  , runExceptT
  )
import Control.Monad.Reader (local)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils
  ( tmpdir
  )
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Ctl.Internal.Wallet.KeyFile (privateStakeKeyFromTextEnvelope)
import Data.Bifunctor (bimap)
import Data.Lens (Lens', (%~))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error, message)
import Internal.CardanoCli as CardanoCli
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Record as Record
import Type.Proxy (Proxy(..))

-- | Changes TextEnvelope type to match private payment key one and tries to read that. 
readTextEnvelopeAsPaymentSkey :: FilePath -> Aff Config.PrivatePaymentKey
readTextEnvelopeAsPaymentSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode payment skey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

-- | Changes TextEnvelope type to match private staking key one and tries to read that. 
readTextEnvelopeAsStakingSkey :: FilePath -> Aff Config.PrivateStakeKey
readTextEnvelopeAsStakingSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = StakeSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode staking skey from decoded envelope")
    $ privateStakeKeyFromTextEnvelope envelope'

type UtxosAtQuery =
  Cardano.Types.Address
  -> Aff (Either Contract.ClientError.ClientError Cardano.Types.UtxoMap)

utxosAtWithCardanoCli
  :: CardanoCli.CardanoNodeInstance
  -> UtxosAtQuery
  -> UtxosAtQuery
utxosAtWithCardanoCli node utxosAt address = runExceptT do
  let
    toCliError :: Error -> Contract.ClientError.ClientError
    toCliError = Contract.ClientError.ClientOtherError <<< message

    toUtxoMap :: Array CardanoCli.CardanoCliTxOutInfo -> Cardano.Types.UtxoMap
    toUtxoMap = Map.fromFoldable
      <<< map (CardanoCli.cardanoCliTxOutInfoToUtxo address)
  cardanoCliUtxos <- ExceptT
    $ map (bimap toCliError toUtxoMap)
    $ try
    $ CardanoCli.queryUtxosViaCardanoCli node address
  kupoUtxos <- ExceptT $ utxosAt address
  pure $ Map.union kupoUtxos cardanoCliUtxos

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
        v872utxoKeysDir = paths.testnetDirectory <</>> "utxo-keys"

      -- a new wallet to send initial lovelace to

      let
        -- | Creates wallet from a payment key in TextEnvelope of any .type as if it was private payment key type.  
        readHackWallet
          :: forall m. MonadAff m => String -> Int -> m (String /\ KeyWallet)
        readHackWallet name idx = liftAff do
          let identifier = name <> show idx
          wallet <- walletFromHackedKeys (identifier <> ".skey") Nothing
          pure $ identifier /\ wallet

        walletFromHackedKeys payment staking = do
          pk <- readTextEnvelopeAsPaymentSkey payment
          sk <- traverse readTextEnvelopeAsStakingSkey staking
          pure $ mkKeyWalletFromPrivateKeys pk sk

      -- do something interesting with every utxo in utxo-keys
      for_ [ 1, 2, 3 ] \idx -> do
        walletName /\ wallet <-
          readHackWallet (v872utxoKeysDir <</>> "utxo") idx
        withKeyWallet wallet do
          let
            nodeCfg =
              { socketPath: paths.nodeSocketPath
              , testnetMagic: cfg.testnetMagic
              }
          -- it will show zero balance
          log
            <<< show
            <<< { wallet: walletName, initialBalanceViaKupo: _ }
            =<< getWalletBalance

          -- we can see genesis utxos when add cardano-cli query results to utxosAt
          local (utxosAtL %~ utxosAtWithCardanoCli nodeCfg) do
            log <<< show <<< { utxosAfterTx: _ } <<< map valueToCoin =<<
              getWalletBalance -- utxosAt newWallet.address

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir

-- | txos
utxosAtL :: Lens' ContractEnv UtxosAtQuery
utxosAtL = prop (Proxy :: _ "handle") <<< prop (Proxy :: _ "utxosAt")

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

