-- | Adapted from `Plutus.V1.Ledger.Time`.
module Types.POSIXTimeRange
  ( Closure
  , Extended(..)
  , Interval(..)
  , LowerBound(..)
  , POSIXTime(..)
  , POSIXTimeRange
  , UpperBound(..)
  , after
  , always
  , before
  , contains
  , from
  , hull
  , intersection
  , interval
  , isEmpty
  , isEmpty' -- Potentially remove and restrict "isEmpty" to BigInt/POSIXTime
  , lowerBound
  , member
  , mkInterval
  , never
  , overlaps
  , overlaps' -- Potentially remove and restrict "overlaps" to BigInt/POSIXTime
  , singleton
  , strictLowerBound
  , strictUpperBound
  , to
  , upperBound
  ) where

import Data.BigInt (BigInt)
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.Maybe (Maybe(Just))
import Data.Show.Generic (genericShow)
import Prelude

--------------------------------------------------------------------------------
-- Interval Type and related
--------------------------------------------------------------------------------
-- | Whether a bound is inclusive or not.
type Closure = Boolean

-- | A set extended with a positive and negative infinity.
data Extended a = NegInf | Finite a | PosInf

derive instance Generic (Extended a) _
derive instance Eq a => Eq (Extended a)
-- Don't change order of Extended of deriving Ord as below
derive instance Ord a => Ord (Extended a)
derive instance Functor Extended

instance Show a => Show (Extended a) where
  show = genericShow

-- | The lower bound of an interval.
data LowerBound a = LowerBound (Extended a) Closure

derive instance Generic (LowerBound a) _
derive instance Eq a => Eq (LowerBound a)
derive instance Functor LowerBound

instance Show a => Show (LowerBound a) where
  show = genericShow

-- Don't derive this as Boolean order will mess up the Closure comparison since
-- false < true.
instance Ord a => Ord (LowerBound a) where
  compare (LowerBound v1 in1) (LowerBound v2 in2) = case v1 `compare` v2 of
    LT -> LT
    GT -> GT
    -- An open lower bound is bigger than a closed lower bound. This corresponds
    -- to the *reverse* of the normal order on Boolean.
    EQ -> in2 `compare` in1

-- | The upper bound of an interval.
data UpperBound a = UpperBound (Extended a) Closure

derive instance Generic (UpperBound a) _
derive instance Eq a => Eq (UpperBound a)
-- Ord is safe to derive because a closed (true) upper bound is greater than
-- an open (false) upper bound and false < true by definition.
derive instance Ord a => Ord (UpperBound a)
derive instance Functor UpperBound
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
derive instance Functor Interval

instance Show a => Show (Interval a) where
  show = genericShow

instance Ord a => JoinSemilattice (Interval a) where
  join = hull

instance Ord a => BoundedJoinSemilattice (Interval a) where
  bottom = never

instance Ord a => MeetSemilattice (Interval a) where
  meet = intersection

instance Ord a => BoundedMeetSemilattice (Interval a) where
  top = always

--------------------------------------------------------------------------------
-- POSIXTIME Type and related
--------------------------------------------------------------------------------
newtype POSIXTime = POSIXTime BigInt

derive instance Generic POSIXTime _
derive newtype instance Eq POSIXTime
derive newtype instance Ord POSIXTime
derive newtype instance Semiring POSIXTime
instance Show POSIXTime where
  show = genericShow

-- | An 'Interval' of 'POSIXTime's.
type POSIXTimeRange = Interval POSIXTime

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
mkInterval :: forall (a :: Type). LowerBound a -> UpperBound a -> Interval a
mkInterval f t = Interval { from: f, to: t }

strictUpperBound :: forall (a :: Type). a -> UpperBound a
strictUpperBound a = UpperBound (Finite a) false

strictLowerBound :: forall (a :: Type). a -> LowerBound a
strictLowerBound a = LowerBound (Finite a) false

lowerBound :: forall (a :: Type). a -> LowerBound a
lowerBound a = LowerBound (Finite a) false

upperBound :: forall (a :: Type). a -> UpperBound a
upperBound a = UpperBound (Finite a) true

-- | @interval a b@ includes all values that are greater than or equal to @a@
-- and smaller than or equal to @b@. Therefore it includes @a@ and @b@.
interval :: forall (a :: Type). a -> a -> Interval a
interval s s' = mkInterval (lowerBound s) (upperBound s')

singleton :: forall (a :: Type). a -> Interval a
singleton s = interval s s

-- | @from a@ is an 'Interval' that includes all values that are
--  greater than or equal to @a@.
from :: forall (a :: Type). a -> Interval a
from s = mkInterval (lowerBound s) (UpperBound PosInf true)

-- | @to a@ is an 'Interval' that includes all values that are
--  smaller than or equal to @a@.
to :: forall (a :: Type). a -> Interval a
to s = mkInterval (LowerBound NegInf true) (upperBound s)

-- | An 'Interval' that covers every slot.
always :: forall (a :: Type). Interval a
always = mkInterval (LowerBound NegInf true) (UpperBound PosInf true)

-- | An 'Interval' that is empty.
never :: forall (a :: Type). Interval a
never = mkInterval (LowerBound PosInf true) (UpperBound NegInf true)

-- | Check whether a value is in an interval.
member :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
member a i = i `contains` singleton a

-- | Check whether two intervals overlap, that is, whether there is a value that
--   is a member of both intervals. This is the Plutus implementation but
-- BigInt used in POSIXTime is cannot be enumerated so the isEmpty we use in
-- practice uses Semiring instead. See `overlaps` for the practical version.
overlaps'
  :: forall (a :: Type). Enum a => Interval a -> Interval a -> Boolean
overlaps' l r = not $ isEmpty' (l `intersection` r)

-- Potential FIX ME: shall we just fix the type to POSIXTime and remove overlaps'
-- and Semiring constraint?
-- | Check whether two intervals overlap, that is, whether there is a value that
--   is a member of both intervals.
overlaps
  :: forall (a :: Type)
   . Ord a
  => Semiring a
  => Interval a
  -> Interval a
  -> Boolean
overlaps l r = not $ isEmpty (l `intersection` r)

-- | 'intersection a b' is the largest interval that is contained in 'a' and in
--   'b', if it exists.
intersection
  :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
intersection (Interval int) (Interval int') =
  mkInterval (max int.from int'.from) (min int.to int'.to)

-- | 'hull a b' is the smallest interval containing 'a' and 'b'.
hull :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
hull (Interval int) (Interval int') =
  mkInterval (min int.from int'.from) (max int.to int'.to)

-- | @a `contains` b@ is true if the 'Interval' @b@ is entirely contained in
--   @a@. That is, @a `contains` b@ if for every entry @s@, if @member s b@ then
--   @member s a@.
contains :: forall (a :: Type). Ord a => Interval a -> Interval a -> Boolean
contains (Interval int) (Interval int') =
  int.from <= int'.from && int'.to <= int.to

-- | Check if an 'Interval' is empty. This is the Plutus implementation but
-- BigInt used in POSIXTime is cannot be enumerated so the isEmpty we use in
-- practice uses Semiring instead. See `isEmpty` for the practical version.
isEmpty' :: forall (a :: Type). Enum a => Interval a -> Boolean
isEmpty' (Interval { from: LowerBound v1 in1, to: UpperBound v2 in2 }) =
  case v1 `compare` v2 of
    LT -> if openInterval then checkEnds v1 v2 else false
    GT -> true
    EQ -> not (in1 && in2)
  where
  openInterval :: Boolean
  openInterval = in1 == false && in2 == false

  -- | We check two finite ends to figure out if there are elements between them.
  -- If there are no elements then the interval is empty.
  checkEnds :: Extended a -> Extended a -> Boolean
  checkEnds (Finite v1') (Finite v2') = (succ v1') `compare` (Just v2') == EQ
  checkEnds _ _ = false

-- Potential FIX ME: shall we just fix the type to POSIXTime and remove isEmpty'
-- and Semiring constraint?
-- | Check if an 'Interval' is empty. This is the practical version to use
-- with a = POSIXTime.
isEmpty :: forall (a :: Type). Ord a => Semiring a => Interval a -> Boolean
isEmpty (Interval { from: LowerBound v1 in1, to: UpperBound v2 in2 }) =
  case v1 `compare` v2 of
    LT -> if openInterval then checkEnds v1 v2 else false
    GT -> true
    EQ -> not (in1 && in2)
  where
  openInterval :: Boolean
  openInterval = in1 == false && in2 == false

  -- | We check two finite ends to figure out if there are elements between them.
  -- If there are no elements then the interval is empty.
  checkEnds :: Extended a -> Extended a -> Boolean
  checkEnds (Finite v1') (Finite v2') = (v1' `add` one) `compare` v2' == EQ
  checkEnds _ _ = false

-- | Check if a value is earlier than the beginning of an 'Interval'.
before :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
before h (Interval { from: f }) = lowerBound h < f

-- | Check if a value is later than the end of a 'Interval'.
-- {-# INLINABLE after #-}
after :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
after h (Interval { to: t }) = upperBound h > t
