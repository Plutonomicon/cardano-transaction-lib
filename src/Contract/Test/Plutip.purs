-- | This module contains everything needed for `Contract` testing in Plutip
-- | environment.
module Contract.Test.Plutip
  ( testPlutipContracts
  , module X
  ) where

import Prelude

import Contract.Monad (runContractInEnv) as X
import Contract.Wallet (withKeyWallet) as X
import Ctl.Internal.Plutip.Server
  ( PlutipTest
  , noWallet
  , runPlutipContract
  , testContractsInEnv
  , withPlutipContractEnv
  , withWallets
  ) as X
import Ctl.Internal.Plutip.Server (PlutipTest, testPlutipContracts) as Server
import Ctl.Internal.Plutip.Types
  ( InitialUTxODistribution
  , InitialUTxOs
  , InitialUTxOsWithStakeKey
  , PlutipConfig
  , UtxoAmount
  ) as X
import Ctl.Internal.Plutip.Types (PlutipConfig)
import Ctl.Internal.Plutip.UtxoDistribution
  ( class UtxoDistribution
  , withStakeKey
  ) as X
import Effect.Aff (Aff)
import Mote (MoteT)

-- | Run `Contract`s in tests in a single Plutip instance.
testPlutipContracts
  :: PlutipConfig
  -> MoteT Aff Server.PlutipTest Aff Unit
  -> MoteT Aff (Aff Unit) Aff Unit
testPlutipContracts = Server.testPlutipContracts
