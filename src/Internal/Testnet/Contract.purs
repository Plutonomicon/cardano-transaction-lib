module Ctl.Internal.Testnet.Contract where

import Contract.Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types as Cardano.Types
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Contract.Monad
  ( Contract
  , ContractEnv
  )
import Contract.Monad as Contract
import Contract.Value (valueToCoin)
import Contract.Wallet
  ( KeyWallet
  , getWalletBalance
  , mkKeyWalletFromPrivateKeys
  , withKeyWallet
  )
import Contract.Wallet as Contract.Wallet
import Control.Monad.Error.Class
  ( liftMaybe
  )
import Ctl.Internal.Plutip.Server
  ( makeClusterContractEnv
  )
import Ctl.Internal.Plutip.Utils
  ( cleanupOnExit
  , whenError
  )
import Ctl.Internal.Test.UtxoDistribution as UtxoDistribution
import Ctl.Internal.Testnet.Server
  ( StartedTestnetCluster
  , startTestnetCluster
  )
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , GenesisUtxoKeyLocation
  , TestnetClusterConfig
  )
import Ctl.Internal.Testnet.Utils (read872GenesisKey)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref

-- | Run a single `Contract` in Plutip environment.
runContract
  :: forall (a :: Type) r
   . Record (TestnetClusterConfig (CardanoTestnetStartupParams r))
  -> Contract a
  -> Aff a
runContract cfg cont = withContractEnv cfg \_ env ->
  Contract.runContractInEnv env cont

readGenesisWallets
  :: forall r r1
   . { genesisKeys :: Array { | GenesisUtxoKeyLocation r } | r1 }
  -> Effect (Array Contract.Wallet.KeyWallet)
readGenesisWallets { genesisKeys } = for genesisKeys \location -> do
  paymentKey <- read872GenesisKey location
  pure $ mkKeyWalletFromPrivateKeys paymentKey Nothing

-- | Provide a `ContractEnv` connected to Plutip.
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
