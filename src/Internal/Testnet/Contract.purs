module Ctl.Internal.Testnet.Contract where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , ContractEnv
  )
import Contract.Monad as Contract
import Ctl.Internal.Plutip.Server
  ( makeClusterContractEnv
  )
import Ctl.Internal.Plutip.Utils
  ( cleanupOnExit
  , whenError
  )
import Ctl.Internal.Testnet.Server
  ( StartedTestnetCluster
  , defaultStartupParams
  , startTestnetCluster
  )
import Ctl.Internal.Testnet.Types (TestnetClusterConfig)
import Effect.Ref as Ref

-- | Run a single `Contract` in Plutip environment.
runContract
  :: forall (a :: Type) r
   . Record (TestnetClusterConfig r)
  -> Contract a
  -> Aff a
runContract cfg cont = withContractEnv cfg \_ env ->
  Contract.runContractInEnv env cont

-- | Provide a `ContractEnv` connected to Plutip.
-- | can be used to run multiple `Contract`s using `runContractInEnv`.
withContractEnv
  :: forall (a :: Type) r
   . Record (TestnetClusterConfig r)
  -> (StartedTestnetCluster -> ContractEnv -> Aff a)
  -> Aff a
withContractEnv cfg cont = do
  cleanupRef <- liftEffect $ Ref.new []
  _ <- cleanupOnExit cleanupRef
  cluster <- startTestnetCluster defaultStartupParams cleanupRef cfg
  { env, printLogs } <- makeClusterContractEnv cleanupRef cfg
  whenError printLogs $ cont cluster env