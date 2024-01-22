module Cardano.Types.Asset where

import Prelude

import Cardano.Types.AssetClass (AssetClass(..))
import Cardano.Types.AssetName (AssetName)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Data.Array.NonEmpty (cons')
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

-- Inspired by cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L485
data Asset = Asset ScriptHash AssetName | AdaAsset

derive instance Eq Asset
derive instance Ord Asset
derive instance Generic Asset _

instance Show Asset where
  show = genericShow

instance Arbitrary Asset where
  arbitrary = oneOf $ cons' (pure AdaAsset) [ Asset <$> arbitrary <*> arbitrary ]

fromAssetClass :: AssetClass -> Asset
fromAssetClass (AssetClass sh tn) = Asset sh tn
