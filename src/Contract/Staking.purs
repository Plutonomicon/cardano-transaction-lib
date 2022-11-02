module Contract.Staking
  ( getPoolIds
  , getPoolParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , getDelegationsAndRewards
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Ctl.Internal.Cardano.Types.Transaction
  ( PoolPubKeyHash
  , PoolRegistrationParams
  )
import Ctl.Internal.Plutus.Types.Credential (Credential)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.QueryM.Pools as QueryM
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.Maybe (Maybe)

getPoolIds :: forall (r :: Row Type). Contract r (Array PoolPubKeyHash)
getPoolIds = wrapContract QueryM.getPoolIds

getPoolParameters
  :: forall (r :: Row Type)
   . PoolPubKeyHash
  -> Contract r PoolRegistrationParams
getPoolParameters poolId = wrapContract $ QueryM.getPoolParameters poolId

getPubKeyHashDelegationsAndRewards
  :: forall (r :: Row Type)
   . StakePubKeyHash
  -> Contract r (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards =
  wrapContract <<< QueryM.getPubKeyHashDelegationsAndRewards

getValidatorHashDelegationsAndRewards
  :: forall (r :: Row Type)
   . StakeValidatorHash
  -> Contract r (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards =
  wrapContract <<< QueryM.getValidatorHashDelegationsAndRewards

getDelegationsAndRewards
  :: forall (r :: Row Type)
   . Credential
  -> Contract r (Maybe DelegationsAndRewards)
getDelegationsAndRewards = wrapContract <<< QueryM.getDelegationsAndRewards
