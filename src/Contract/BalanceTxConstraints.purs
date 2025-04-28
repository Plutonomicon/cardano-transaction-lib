-- | A module that defines several balancer constraints that can be used to
-- | adjust the behaviour of the balancer.
module Contract.BalanceTxConstraints (module BalanceTxConstraints) where

import Cardano.Transaction.Balancer.Constraints
  ( BalanceTxConstraintsBuilder
  , BalancerConfig(BalancerConfig)
  , BalancerConstraints(BalancerConstraints)
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxoWithOutRef
  , mustNotSpendUtxosWithOutRefs
  , mustSendChangeToAddress
  , mustSendChangeWithDatum
  , mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddress
  , mustUseUtxosAtAddresses
  ) as BalanceTxConstraints
