module Contract.Staking
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , getStakeCredentialDelegationsAndRewards
  , module X
  ) where

import Prelude

import Cardano.Types
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , Ed25519KeyHash
  , PoolPubKeyHash
  , ScriptHash
  , StakeCredential(StakeCredential)
  )
import Contract.Monad (Contract)
import Control.Monad.Reader (asks)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards)
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards) as X
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

getStakeCredentialDelegationsAndRewards
  :: StakeCredential
  -> Contract (Maybe DelegationsAndRewards)
getStakeCredentialDelegationsAndRewards = case _ of
  StakeCredential (PubKeyHashCredential pkh) ->
    getPubKeyHashDelegationsAndRewards pkh
  StakeCredential (ScriptHashCredential sh) ->
    getValidatorHashDelegationsAndRewards sh

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
