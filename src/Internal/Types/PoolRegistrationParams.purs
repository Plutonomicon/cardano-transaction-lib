module Ctl.Internal.Types.PoolRegistrationParams where

import Cardano.Plutus.Types.PubKeyHash (PubKeyHash)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.PoolMetadata (PoolMetadata)
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.Relay (Relay)
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.VRFKeyHash (VRFKeyHash)
import Data.Maybe (Maybe)

type PoolRegistrationParams =
  { operator :: PoolPubKeyHash -- cwitness (cert)
  , vrfKeyhash :: VRFKeyHash
  -- needed to prove that the pool won the lottery
  , pledge :: BigNum
  , cost :: BigNum -- >= pparams.minPoolCost
  , margin :: UnitInterval -- proportion that goes to the reward account
  , rewardAccount :: RewardAddress
  , poolOwners :: Array PubKeyHash
  -- payment key hashes that contribute to pledge amount
  , relays :: Array Relay
  , poolMetadata :: Maybe PoolMetadata
  }
