-- | Adapted from `Plutus.V1.Ledger.Time`.
module Types.POSIXTimeRange
  ( Closure
  , Extended(..)
  , LowerBound(..)
  , UpperBound(..)
  , Interval(..)
  , POSIXTime(..)
  , POSIXTimeRange
  ) where

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude

-- | Whether a bound is inclusive or not.
type Closure = Boolean

-- | A set extended with a positive and negative infinity.
data Extended a = NegInf | Finite a | PosInf

derive instance Generic (Extended a) _
derive instance Eq a => Eq (Extended a)

instance Show a => Show (Extended a) where
  show = genericShow

-- | The lower bound of an interval.
data LowerBound a = LowerBound (Extended a) Closure

derive instance Generic (LowerBound a) _
derive instance Eq a => Eq (LowerBound a)
instance Show a => Show (LowerBound a) where
  show = genericShow

-- | The upper bound of an interval.
data UpperBound a = UpperBound (Extended a) Closure

derive instance Generic (UpperBound a) _
derive instance Eq a => Eq (UpperBound a)
instance Show a => Show (UpperBound a) where
  show = genericShow

-- | An interval of @a@s.
-- |
-- | The interval may be either closed or open at either end, meaning
-- | that the endpoints may or may not be included in the interval.
-- |
-- | The interval can also be unbounded on either side.
newtype Interval a = Interval { from :: LowerBound a, to :: UpperBound a }

derive instance Generic (Interval a) _
derive newtype instance Eq a => Eq (Interval a)
instance Show a => Show (Interval a) where
  show = genericShow

newtype POSIXTime = POSIXTime BigInt

derive instance Generic POSIXTime _
derive newtype instance Eq POSIXTime
instance Show POSIXTime where
  show = genericShow

-- | An 'Interval' of 'POSIXTime's.
type POSIXTimeRange = Interval POSIXTime
