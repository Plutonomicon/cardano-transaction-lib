module Internal.Testnet.Contract where

import Contract.Prelude

import Contract.Config as Config
import Contract.Monad (ContractEnv)
import Ctl.Internal.Logging (Logger)
import Ctl.Internal.Plutip.Server (mkClusterContractEnv)
import Internal.Testnet.Types (TestnetClusterConfig)

makeContractEnv
  :: forall r
   . Record (TestnetClusterConfig r)
  -> Logger
  -> Maybe (LogLevel -> Config.Message -> Aff Unit)
  -> Aff ContractEnv
makeContractEnv = mkClusterContractEnv
