module Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPoolParameters
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , DelegationsAndRewards
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( PoolPubKeyHash
  , PoolRegistrationParams
  )
import Ctl.Internal.Cardano.Types.Value (Coin)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios
  ( DelegationsAndRewardsR(DelegationsAndRewardsR)
  , PoolParametersR(PoolParametersR)
  )
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , scriptHashToBech32Unsafe
  )
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Exception (error)
import Record.Builder (build, merge)

getPoolIds :: QueryM (Array PoolPubKeyHash)
getPoolIds = mkOgmiosRequest Ogmios.queryPoolIdsCall
  _.poolIds
  unit

-- TODO: batched variant
getPoolParameters :: PoolPubKeyHash -> QueryM PoolRegistrationParams
getPoolParameters poolPubKeyHash = do
  PoolParametersR params <- mkOgmiosRequest Ogmios.queryPoolParameters
    _.poolParameters
    [ poolPubKeyHash ]
  poolIdStr <-
    liftM (error "Unable to encode pool pubkey hash to bech32")
      $ ed25519KeyHashToBech32 "pool"
      $ unwrap poolPubKeyHash
  res <- liftM (error "Unable to find pool ID in the response") $ Map.lookup
    poolIdStr
    params
  pure $ build
    ( merge
        { operator: poolPubKeyHash
        , poolOwners: res.poolOwners <#> wrap >>> wrap
        }
    )
    res

type DelegationsAndRewards =
  { rewards :: Maybe Coin
  , delegate :: Maybe PoolPubKeyHash
  }

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
  stringRep = scriptHashToBech32Unsafe "script" $ unwrap skh

  byteHex :: String
  byteHex =
    byteArrayToHex <<< unwrap <<< toBytes <<< unwrap $
      skh

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
  stringRep =
    ed25519KeyHashToBech32Unsafe "stake_vkh" $ unwrap $ unwrap pkh

  byteHex :: String
  byteHex =
    byteArrayToHex <<< unwrap <<< toBytes <<< unwrap $
      unwrap
        pkh
