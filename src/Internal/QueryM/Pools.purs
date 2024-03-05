module Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPoolParameters
  , getPoolsParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , module X
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (PoolPubKeyHash, StakePubKeyHash)
import Cardano.Types.Ed25519KeyHash (toBech32Unsafe) as Ed25519KeyHash
import Cardano.Types.ScriptHash as ScriptHash
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios
  ( DelegationsAndRewardsR(DelegationsAndRewardsR)
  , PoolParameters
  )
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards)
import Ctl.Internal.Types.DelegationsAndRewards (DelegationsAndRewards) as X
import Ctl.Internal.Types.PoolRegistrationParams (PoolRegistrationParams)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.ByteArray (byteArrayToHex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Record.Builder (build, merge)

-- | Get pool parameters of all pools or of the provided pools.
getStakePools
  :: (Maybe (Array PoolPubKeyHash))
  -> QueryM (Map PoolPubKeyHash PoolParameters)
getStakePools selected = unwrap <$>
  mkOgmiosRequest Ogmios.queryStakePoolsCall
    _.stakePools
    (wrap selected)

getPoolIds :: QueryM (Array PoolPubKeyHash)
getPoolIds = (Map.toUnfoldableUnordered >>> map fst) <$>
  getStakePools Nothing

getPoolParameters :: PoolPubKeyHash -> QueryM PoolRegistrationParams
getPoolParameters poolPubKeyHash = do
  params <- getPoolsParameters [ poolPubKeyHash ]
  res <- liftM (error "Unable to find pool ID in the response") $ Map.lookup
    poolPubKeyHash
    params
  pure res

getPoolsParameters
  :: Array PoolPubKeyHash -> QueryM (Map PoolPubKeyHash PoolRegistrationParams)
getPoolsParameters poolPubKeyHashes = do
  response <- getStakePools (Just poolPubKeyHashes)
  pure $ Map.mapMaybeWithKey
    ( \poolPkh params -> Just $ build
        ( merge
            { operator: poolPkh
            , poolOwners: params.poolOwners <#> wrap
            }
        )
        params
    )
    response

getValidatorHashDelegationsAndRewards
  :: StakeValidatorHash -> QueryM (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards skh = do
  DelegationsAndRewardsR mp <- mkOgmiosRequest Ogmios.queryDelegationsAndRewards
    _.delegationsAndRewards
    [ stringRep
    ]
  pure $ Map.lookup byteHex mp
  where
  stringRep :: String
  stringRep = unsafePartial $ ScriptHash.toBech32Unsafe "script" $ unwrap skh

  byteHex :: String
  byteHex = byteArrayToHex $ unwrap $ encodeCbor $ unwrap skh

-- TODO: batched variant
getPubKeyHashDelegationsAndRewards
  :: StakePubKeyHash -> QueryM (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards pkh = do
  DelegationsAndRewardsR mp <- mkOgmiosRequest Ogmios.queryDelegationsAndRewards
    _.delegationsAndRewards
    [ stringRep ]
  pure $ Map.lookup byteHex mp
  where
  stringRep :: String
  stringRep = unsafePartial
    $ Ed25519KeyHash.toBech32Unsafe "stake_vkh"
    $ unwrap pkh

  byteHex :: String
  byteHex = byteArrayToHex $ unwrap $ encodeCbor
    $ unwrap pkh
