module Contract.Staking
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , module X
  ) where

import Prelude

import Cardano.Types (PoolPubKeyHash, StakePubKeyHash)
import Contract.Monad (Contract)
import Control.Monad.Reader (asks)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards) as X
import Ctl.Internal.Types.StakeValidatorHash (StakeValidatorHash)
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

getPoolIds :: Contract (Array PoolPubKeyHash)
getPoolIds = do
  queryHandle <- getQueryHandle
  liftAff $
    queryHandle.getPoolIds
      >>= either (liftEffect <<< throw <<< show) pure

getPubKeyHashDelegationsAndRewards
  :: StakePubKeyHash
  -> Contract (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards stakePubKeyHash = do
  queryHandle <- getQueryHandle
  networkId <- asks _.networkId
  liftAff do
    queryHandle.getPubKeyHashDelegationsAndRewards networkId stakePubKeyHash
      >>= either (liftEffect <<< throw <<< show) pure

getValidatorHashDelegationsAndRewards
  :: StakeValidatorHash
  -> Contract (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards stakeValidatorHash = do
  queryHandle <- getQueryHandle
  networkId <- asks _.networkId
  liftAff do
    queryHandle.getValidatorHashDelegationsAndRewards networkId
      (unwrap stakeValidatorHash)
      >>= either (liftEffect <<< throw <<< show) pure
