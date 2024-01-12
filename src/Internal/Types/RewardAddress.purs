module Ctl.Internal.Types.RewardAddress
  ( RewardAddress(RewardAddress)
  , rewardAddressFromBytes
  , rewardAddressToBytes
  , rewardAddressFromBech32
  , rewardAddressToBech32
  , stakePubKeyHashRewardAddress
  , stakeValidatorHashRewardAddress
  , unRewardAddress
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Serialization.Address
  ( keyHashCredential
  , scriptHashCredential
  )
import Ctl.Internal.Serialization.Address as Csl
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)

newtype RewardAddress = RewardAddress Csl.RewardAddress

derive newtype instance Eq RewardAddress
derive newtype instance Ord RewardAddress
derive newtype instance Show RewardAddress
derive newtype instance FromData RewardAddress
derive newtype instance ToData RewardAddress
derive newtype instance EncodeAeson RewardAddress
derive newtype instance DecodeAeson RewardAddress

unRewardAddress :: RewardAddress -> Csl.RewardAddress
unRewardAddress (RewardAddress address) = address

rewardAddressFromBytes :: ByteArray -> Maybe RewardAddress
rewardAddressFromBytes = wrap >>> Csl.rewardAddressFromBytes >>> map
  RewardAddress

rewardAddressFromBech32 :: Bech32String -> Maybe RewardAddress
rewardAddressFromBech32 = Csl.rewardAddressFromBech32 >>> map RewardAddress

rewardAddressToBech32 :: RewardAddress -> Bech32String
rewardAddressToBech32 = unRewardAddress >>> Csl.rewardAddressBech32

rewardAddressToBytes :: RewardAddress -> CborBytes
rewardAddressToBytes = unRewardAddress >>> Csl.rewardAddressBytes

stakePubKeyHashRewardAddress
  :: Csl.NetworkId -> StakePubKeyHash -> RewardAddress
stakePubKeyHashRewardAddress networkId pkh = RewardAddress $ Csl.rewardAddress
  { network: networkId
  , paymentCred: keyHashCredential $ unwrap $ unwrap pkh
  }

stakeValidatorHashRewardAddress
  :: Csl.NetworkId -> StakeValidatorHash -> RewardAddress
stakeValidatorHashRewardAddress networkId sh = RewardAddress $ Csl.rewardAddress
  { network: networkId
  , paymentCred: scriptHashCredential (unwrap sh)
  }
