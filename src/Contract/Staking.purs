module Contract.Staking
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , module X
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash, PoolPubKeyHash, ScriptHash)
import Contract.Monad (Contract)
import Control.Monad.Reader (asks)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards) as X
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
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
  :: Ed25519KeyHash
  -> Contract (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards stakePubKeyHash = do
  queryHandle <- getQueryHandle
  networkId <- asks _.networkId
  liftAff do
    queryHandle.getPubKeyHashDelegationsAndRewards networkId
      (wrap stakePubKeyHash)
      >>= either (liftEffect <<< throw <<< show) pure

getValidatorHashDelegationsAndRewards
  :: ScriptHash
  -> Contract (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards stakeValidatorHash = do
  queryHandle <- getQueryHandle
  networkId <- asks _.networkId
  liftAff do
    queryHandle.getValidatorHashDelegationsAndRewards networkId
      stakeValidatorHash
      >>= either (liftEffect <<< throw <<< show) pure
