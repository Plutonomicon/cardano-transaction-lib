module Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  ) where

import Prelude

import Cardano.Types (Coin, CostModel, Epoch, ExUnitPrices, ExUnits, Language)
import Ctl.Internal.Types.Rational (Rational)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)

-- Based on `Cardano.Api.ProtocolParameters.ProtocolParameters` from
-- `cardano-api`.
newtype ProtocolParameters = ProtocolParameters
  { protocolVersion :: UInt /\ UInt
  , decentralization :: Rational
  , maxBlockHeaderSize :: UInt
  , maxBlockBodySize :: UInt
  , maxTxSize :: UInt
  , txFeeFixed :: Coin
  , txFeePerByte :: UInt
  , stakeAddressDeposit :: Coin
  , stakePoolDeposit :: Coin
  , minPoolCost :: Coin
  , poolRetireMaxEpoch :: Epoch
  , stakePoolTargetNum :: UInt
  , poolPledgeInfluence :: Rational
  , monetaryExpansion :: Rational
  , treasuryCut :: Rational
  , coinsPerUtxoByte :: Coin
  , costModels :: Map Language CostModel
  , prices :: ExUnitPrices
  , maxTxExUnits :: ExUnits
  , maxBlockExUnits :: ExUnits
  , maxValueSize :: UInt
  , collateralPercent :: UInt
  , maxCollateralInputs :: UInt
  , govActionDeposit :: Coin
  , drepDeposit :: Coin
  , refScriptCoinsPerByte :: Rational
  }

derive instance Newtype ProtocolParameters _
derive instance Generic ProtocolParameters _
derive instance Eq ProtocolParameters

instance Show ProtocolParameters where
  show = genericShow
