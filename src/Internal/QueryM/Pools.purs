module Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPoolParameters
  , getPoolsParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (PoolParams, PoolPubKeyHash, StakePubKeyHash)
import Cardano.Types.DelegationsAndRewards (DelegationsAndRewards)
import Cardano.Types.Ed25519KeyHash (toBech32Unsafe) as Ed25519KeyHash
import Cardano.Types.ScriptHash as ScriptHash
import Cardano.Types.StakeValidatorHash (StakeValidatorHash)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types
  ( PoolParameters
  , pprintOgmiosDecodeError
  )
import Data.ByteArray (byteArrayToHex)
import Data.Either (either)
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
  :: Maybe (Array PoolPubKeyHash)
  -> QueryM (Map PoolPubKeyHash PoolParameters)
getStakePools selected =
  Ogmios.poolParameters (wrap selected) >>= either
    (throwError <<< error <<< pprintOgmiosDecodeError)
    (pure <<< unwrap)

getPoolIds :: QueryM (Array PoolPubKeyHash)
getPoolIds = (Map.toUnfoldableUnordered >>> map fst) <$>
  getStakePools Nothing

getPoolParameters :: PoolPubKeyHash -> QueryM PoolParams
getPoolParameters poolPubKeyHash = do
  params <- getPoolsParameters [ poolPubKeyHash ]
  res <- liftM (error "Unable to find pool ID in the response") $ Map.lookup
    poolPubKeyHash
    params
  pure res

getPoolsParameters
  :: Array PoolPubKeyHash -> QueryM (Map PoolPubKeyHash PoolParams)
getPoolsParameters poolPubKeyHashes = do
  response <- getStakePools (Just poolPubKeyHashes)
  pure $ Map.mapMaybeWithKey
    ( \poolPkh params -> Just $ wrap $ build
        ( merge
            { operator: poolPkh
            , poolOwners: params.poolOwners
            }
        )
        params
    )
    response

getValidatorHashDelegationsAndRewards
  :: StakeValidatorHash -> QueryM (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards skh =
  Ogmios.delegationsAndRewards [ stringRep ] >>= either
    (throwError <<< error <<< pprintOgmiosDecodeError)
    (pure <<< Map.lookup byteHex <<< unwrap)
  where
  stringRep :: String
  stringRep = unsafePartial $ ScriptHash.toBech32Unsafe "script" $ unwrap skh

  byteHex :: String
  byteHex = byteArrayToHex $ unwrap $ encodeCbor $ unwrap skh

-- TODO: batched variant
getPubKeyHashDelegationsAndRewards
  :: StakePubKeyHash -> QueryM (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards pkh =
  Ogmios.delegationsAndRewards [ stringRep ] >>= either
    (throwError <<< error <<< pprintOgmiosDecodeError)
    (pure <<< Map.lookup byteHex <<< unwrap)
  where
  stringRep :: String
  stringRep = unsafePartial
    $ Ed25519KeyHash.toBech32Unsafe "stake_vkh"
    $ unwrap pkh

  byteHex :: String
  byteHex = byteArrayToHex $ unwrap $ encodeCbor
    $ unwrap pkh
