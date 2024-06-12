module Ctl.Internal.Testnet.Contract where

import Contract.Prelude

import Cardano.Types (NetworkId(TestnetId))
import Contract.Monad
  ( Contract
  , ContractEnv
  )
import Contract.Monad as Contract
import Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  )
import Contract.Wallet as Contract.Wallet
import Ctl.Internal.Plutip.Server
  ( makeClusterContractEnv
  )
import Ctl.Internal.Plutip.Utils
  ( cleanupOnExit
  , whenError
  )
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