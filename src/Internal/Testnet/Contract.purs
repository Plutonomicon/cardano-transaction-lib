module Ctl.Internal.Testnet.Contract where

import Contract.Prelude

import Cardano.Serialization.Lib (privateKey_generateEd25519) as Csl
import Cardano.Serialization.Lib as CSL
import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types as Cardano.Types
import Cardano.Types.Address (Address, getPaymentCredential, getStakeCredential)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(..))
import Cardano.Types.PaymentCredential (PaymentCredential(..))
import Cardano.Types.StakeCredential (StakeCredential(..))
import Cardano.Types.StakePubKeyHash (StakePubKeyHash(..))
import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, liftContractM, liftedM, runContractInEnv)
import Contract.Monad as Contract
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (TxConstraints(..))
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (Value(..), valueToCoin)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (KeyWallet, getWalletAddress, getWalletBalance, mkKeyWalletFromPrivateKeys, withKeyWallet)
import Contract.Wallet as Contract.Wallet
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddress)
import Ctl.Internal.Plutip.Server (makeClusterContractEnv)
import Ctl.Internal.Plutip.Utils (cleanupOnExit, runCleanup, whenError)
import Ctl.Internal.Test.UtxoDistribution (class UtxoDistribution, UtxoAmount, decodeWallets, encodeDistribution, keyWallets)
import Ctl.Internal.Test.UtxoDistribution as UtxoDistribution
import Ctl.Internal.Testnet.Server (StartedTestnetCluster, startTestnetCluster)
import Ctl.Internal.Testnet.Types (CardanoTestnetStartupParams, GenesisUtxoKeyLocation, TestnetClusterConfig)
import Ctl.Internal.Testnet.Utils (read872GenesisKey)
import Ctl.Internal.Wallet.Key (KeyWallet(..))
import Data.Array (head)
import Data.Array (zip) as Array
import Data.Maybe (fromJust)
import Debug (spy, traceM)
import Effect.Aff (bracket) as Aff
import Effect.Aff (try)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Internal.CardanoCli.QueryHandler (withCardanoCliCompletion) as CardanoCli.QueryHandler
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

-- | Run a single `Contract` in cardano-testnet environment.
runContract
  :: forall (a :: Type) r
   . Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
  -> Contract a
  -> Aff a
runContract cfg cont = withContractEnv cfg \_ env ->
  Contract.runContractInEnv env cont

-- | Run a single `Contract` in cardano-testnet environment.
runTestnetContract
  :: forall (distr :: Type) (wallets :: Type) (r :: Row Type) (a :: Type)
   . UtxoDistribution distr wallets
  => Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
  -> distr
  -> (wallets -> Contract a)
  -> Aff a
runTestnetContract cfg distr cont =
  withTestnetContractEnv cfg distr \env wallets ->
    runContractInEnv env (cont wallets)

readGenesisWallets
  :: forall r r1
   . { genesisKeys :: Array { | GenesisUtxoKeyLocation r } | r1 }
  -> Effect (Array Contract.Wallet.KeyWallet)
readGenesisWallets { genesisKeys } = for genesisKeys \location -> do
  paymentKey <- read872GenesisKey location
  pure $ mkKeyWalletFromPrivateKeys paymentKey Nothing

withTestnetContractEnv
  :: forall (distr :: Type) (wallets :: Type) (r :: Row Type) (a :: Type)
   . UtxoDistribution distr wallets
  => Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
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

-- | Provide a `ContractEnv` connected to cardano-testnet.
-- | can be used to run multiple `Contract`s using `runContractInEnv`.
startTestnetContractEnv
  :: forall (distr :: Type) (wallets :: Type) (r :: Row Type)
   . UtxoDistribution distr wallets
  => Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
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
  cleanupRef <- liftEffect $ Ref.new []
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
      let testnetPaths = (unwrap cluster).paths
      genesisWallets <- liftEffect $ readGenesisWallets testnetPaths
      let genesisWallet = unsafePartial fromJust $ head genesisWallets -- FIXME
      withKeyWallet genesisWallet do
        let distrArray = encodeDistribution distr 
        privateKeys <-
          for (encodeDistribution distr) \_ ->
            liftEffect $ wrap <$> Csl.privateKey_generateEd25519
        wallets <-
          liftContractM "Impossible happened: could not decode wallets. Please report as bug"
            $ decodeWallets distr privateKeys
        let kws = keyWallets (Proxy :: _ distr) wallets
        genesisAddr <- liftedM "Could not get genesis address" getWalletAddress
        let
          nodeCfg =
            { socketPath: (unwrap cluster).paths.nodeSocketPath
            , testnetMagic: cfg.testnetMagic
            }
        CardanoCli.QueryHandler.withCardanoCliCompletion nodeCfg genesisAddr do
          let walletsAmounts = Array.zip kws distrArray
          fundWalletsFromGenesis genesisWallet (spy "walletsAmounts" walletsAmounts)
        pure wallets

  fundWalletsFromGenesis :: KeyWallet -> Array (KeyWallet /\ Array UtxoAmount) -> Contract Unit
  fundWalletsFromGenesis genesisWallet walletsAmounts = do
    network <- getNetworkId
    let
      constraints :: TxConstraints
      constraints =
        foldMap
          ( \(KeyWallet kw /\ amounts) ->
              foldMap (mustPayToAddress (kw.address network) <<< Value.lovelaceValueOf)
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
        Just (StakeCredential (PubKeyHashCredential skh)) -> Just $ StakePubKeyHash skh
        _ -> Nothing
    in
      case getPaymentCredential addr of
        Just (PaymentCredential (PubKeyHashCredential pkh)) ->
          mustPayToPubKeyStakeAddress (wrap pkh) skh
        _ -> mempty
    
-- | Provide a `ContractEnv` connected to cardano-testnet.
-- | can be used to run multiple `Contract`s using `runContractInEnv`.
withContractEnv
  :: forall (a :: Type) r
   . Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
  -> (StartedTestnetCluster -> ContractEnv -> Aff a)
  -> Aff a
withContractEnv cfg cont = do
  cleanupRef <- liftEffect $ Ref.new []
  _ <- cleanupOnExit cleanupRef
  cluster <- startTestnetCluster cfg cleanupRef
  { env, printLogs } <- makeClusterContractEnv cleanupRef cfg
  whenError printLogs $ cont cluster $ env { networkId = TestnetId }

distributeFunds
  :: { sources :: Array KeyWallet
     , targets ::
         Array
           { wallet :: KeyWallet
           , required ::
               { totalRequired :: BigNum
               , utxoDistribution :: Array BigNum
               }
           }
     , thresholds ::
         { maxCoinPerTx :: BigNum
         , maxUtxosPerTx :: BigNum
         }
     }
  -> Contract Unit
distributeFunds given = do
  sources <- for given.sources \wallet -> do
    balance <- withKeyWallet wallet
      $ map (BigNum.toBigInt <<< unwrap <<< valueToCoin)
      <<< liftMaybe (error "Can't get source wallet balance")
      =<< getWalletBalance
    pure { wallet, balance }
  unsafeThrow "todo"

makeTargetDistribution
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution.UtxoDistribution distr wallets
  => distr
  -> Effect
       { wallets :: wallets
       , distributionInfo ::
           Array
             { wallet :: KeyWallet
             , required ::
                 { total :: BigNum
                 , utxoDistribution :: Array BigNum
                 }
             }
       }
makeTargetDistribution distrib = do
  distributionInfo <- for (UtxoDistribution.encodeDistribution distrib)
    \utxos -> do
      paymentKey <- wrap <$> genPrivateKey
      let
        wallet = mkKeyWalletFromPrivateKeys paymentKey Nothing
      total <- liftMaybe (error "Cannot sum target utxo amounts")
        $ foldM BigNum.add BigNum.zero utxos
      pure
        { wallet
        , required: { utxoDistribution: utxos, total }
        }
  let
    paymentKeys =
      unwrap
        <<< _.paymentKey
        <<< unwrap
        <<< _.wallet
        <$> distributionInfo
  wallets <-
    liftMaybe (error "Can't decode wallets")
      $ UtxoDistribution.decodeWallets distrib paymentKeys
  pure { wallets, distributionInfo }

genPrivateKey :: Effect Cardano.Types.PrivateKey
genPrivateKey = wrap <$> CSL.privateKey_generateEd25519
