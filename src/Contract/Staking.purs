module Contract.Staking
  ( getPoolIds
  , getPoolParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Cardano.Types.Transaction
  ( PoolPubKeyHash
  , PoolRegistrationParams
  )
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards) as X
import Ctl.Internal.QueryM.Pools as QueryM
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.Maybe (Maybe)

getPoolIds :: Contract (Array PoolPubKeyHash)
getPoolIds = wrapQueryM QueryM.getPoolIds

getPoolParameters
  :: PoolPubKeyHash
  -> Contract PoolRegistrationParams
getPoolParameters = wrapQueryM <<< QueryM.getPoolParameters

getPubKeyHashDelegationsAndRewards
  :: StakePubKeyHash
  -> Contract (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards =
  wrapQueryM <<< QueryM.getPubKeyHashDelegationsAndRewards

getValidatorHashDelegationsAndRewards
  :: StakeValidatorHash
  -> Contract (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards =
  wrapQueryM <<< QueryM.getValidatorHashDelegationsAndRewards
