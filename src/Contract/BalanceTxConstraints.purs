-- | A module that defines several balancer constraints that can be used to
-- | adjust the behaviour of the balancer.
module Contract.BalanceTxConstraints (module BalanceTxConstraints) where

import Ctl.Internal.BalanceTx.Constraints
  ( BalancerConstraints
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxoWithOutRef
  , mustNotSpendUtxosWithOutRefs
  , mustSendChangeToAddress
  , mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseUtxosAtAddress
  , mustUseUtxosAtAddresses
  ) as BalanceTxConstraints
