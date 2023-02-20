module Ctl.Internal.Test.ContractTest
  ( ContractTest(ContractTest)
  , withWallets
  , noWallet
  , ContractTestHandler
  , ContractTestPlan(ContractTestPlan)
  , ContractTestPlanHandler
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.UtxoDistribution (class UtxoDistribution)

-- | Represents a `Contract` test suite that depend on *some* wallet
-- | `UtxoDistribution`.
newtype ContractTest = ContractTest
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestHandler distr wallets r
       )
    -> r
  )

-- | Store a wallet `UtxoDistribution` and a `Contract` that depends on those wallets
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract Unit)
  -> ContractTest
withWallets distr tests = ContractTest \h -> h distr tests

-- | Lift a `Contract` into `ContractTest`
noWallet :: Contract Unit -> ContractTest
noWallet = withWallets unit <<< const

-- | A runner for a test suite that supports funds distribution.
type ContractTestHandler :: Type -> Type -> Type -> Type
type ContractTestHandler distr wallets r =
  UtxoDistribution distr wallets => distr -> (wallets -> Contract Unit) -> r

-- | Represents `Contract`s in `TestPlanM` that depend on *some* wallet `UtxoDistribution`
newtype ContractTestPlan = ContractTestPlan
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestPlanHandler distr wallets r
       )
    -> r
  )

-- | Same as `ContractTestHandler`, but wrapped in a `TestPaln`.
type ContractTestPlanHandler :: Type -> Type -> Type -> Type
type ContractTestPlanHandler distr wallets r =
  UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> r
