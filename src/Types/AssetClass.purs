module Cardano.Types.AssetClass where


import Prelude hiding (join)

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.MultiAsset (MultiAsset(..), pprintMultiAsset, unionNonAda, unionWithNonAda)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.ScriptHash (ScriptHash)
import Ctl.Internal.Partition (class Equipartition, equipartition)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lattice (class JoinSemilattice, class MeetSemilattice, join, meet)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Test.QuickCheck (class Arbitrary, arbitrary)

--------------------------------------------------------------------------------
-- AssetClass
--------------------------------------------------------------------------------

data AssetClass = AssetClass ScriptHash AssetName

derive instance Generic AssetClass _
derive instance Eq AssetClass
derive instance Ord AssetClass

instance Arbitrary AssetClass where
  arbitrary = AssetClass <$> arbitrary <*> arbitrary

instance Show AssetClass where
  show = genericShow
