module Ctl.Internal.Test.ContractTest
  ( ContractTest(ContractTest)
  , ContractTestHandler
  , ContractTestPlan(ContractTestPlan)
  , ContractTestPlanHandler
  , noWallet
  , noWalletExposeClusterParams
  , sameWallets
  , sameWalletsExposeClusterParams
  , withWallets
  , withWalletsExposeClusterParams
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Plutip.Types (ClusterStartupParameters)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.UtxoDistribution (class UtxoDistribution)
import Data.Maybe (Maybe(Nothing))
import Mote.Monad (mapTest)

-- | Represents a `Contract` test suite that depend on *some* wallet
-- | `UtxoDistribution`.
-- Internally this function takes a two-argument callback from
-- some distribution and a single test to some value and returns that value.
-- Another way of looking at it: pattern-match `ContractTest runTest`,
-- then you can pass a function to `runTest`:
-- `runTest \distr test -> ...` which gets you a result.
-- In practice `runTest` is a closure that stores distribution and a test and
-- passes them to the (\distr test -> ...) function.
newtype ContractTest = ContractTest
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestHandler distr wallets r
       )
    -> r
  )

withWalletsExposeClusterParams
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (Maybe ClusterStartupParameters -> wallets -> Contract Unit)
  -> ContractTest
withWalletsExposeClusterParams distr tests = ContractTest \h -> h distr tests

-- | Store a wallet `UtxoDistribution` and a `Contract` that depends on those wallets
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract Unit)
  -> ContractTest
withWallets distr = withWalletsExposeClusterParams distr <<< const

noWalletExposeClusterParams
  :: (Maybe ClusterStartupParameters -> Contract Unit)
  -> ContractTest
noWalletExposeClusterParams tests =
  withWalletsExposeClusterParams unit \clusterParams _ ->
    tests clusterParams

-- | Lift a `Contract` into `ContractTest`
noWallet :: Contract Unit -> ContractTest
noWallet = noWalletExposeClusterParams <<< const

sameWalletsExposeClusterParams
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> TestPlanM (Maybe ClusterStartupParameters -> wallets -> Contract Unit) Unit
  -> ContractTestPlan
sameWalletsExposeClusterParams distr tests = ContractTestPlan \h -> h distr
  tests

-- | Store a wallet `UtxoDistribution` and a `TestPlanM` that depend on those wallets
sameWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> ContractTestPlan
sameWallets distr =
  sameWalletsExposeClusterParams distr
    <<< mapTest (\test -> (\clusterParams -> test))

-- | A runner for a test suite that supports funds distribution.
type ContractTestHandler :: Type -> Type -> Type -> Type
type ContractTestHandler distr wallets r =
  UtxoDistribution distr wallets
  => distr
  -> (Maybe ClusterStartupParameters -> wallets -> Contract Unit)
  -> r

-- | Represents `Contract`s in `TestPlanM` that depend on *some* wallet `UtxoDistribution`
-- Internally this is similar to `ContractTest`, except that
-- now a `runGroupPlan` (a function wrapped in the `ContractTestPlan`) closure
-- stores distribution and effects to construct a test tree.
newtype ContractTestPlan = ContractTestPlan
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestPlanHandler distr wallets r
       )
    -> r
  )

-- | Same as `ContractTestHandler`, but wrapped in a `TestPlanM`.
-- | It is used for the reconstruction of the `MoteT` value.
-- | See the `Ctl.Internal.Plutip.execDistribution` function for more info.
type ContractTestPlanHandler :: Type -> Type -> Type -> Type
type ContractTestPlanHandler distr wallets r =
  UtxoDistribution distr wallets
  => distr
  -> TestPlanM (Maybe ClusterStartupParameters -> wallets -> Contract Unit) Unit
  -> r
