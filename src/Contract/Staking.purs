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
import Cardano.Types.DelegationsAndRewards (DelegationsAndRewards)
import Cardano.Types.DelegationsAndRewards (DelegationsAndRewards) as X
import Contract.Monad (Contract)
import Control.Monad.Reader (asks)
import Ctl.Internal.Contract.Monad (getProvider)
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

getPoolIds :: Contract (Array PoolPubKeyHash)
getPoolIds = do
  provider <- getProvider
  liftAff $
    provider.getPoolIds
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
  provider <- getProvider
  networkId <- asks _.networkId
  liftAff do
    provider.getPubKeyHashDelegationsAndRewards networkId
      (wrap stakePubKeyHash)
      >>= either (liftEffect <<< throw <<< show) pure

getValidatorHashDelegationsAndRewards
  :: ScriptHash
  -> Contract (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards stakeValidatorHash = do
  provider <- getProvider
  networkId <- asks _.networkId
  liftAff do
    provider.getValidatorHashDelegationsAndRewards networkId
      stakeValidatorHash
      >>= either (liftEffect <<< throw <<< show) pure
