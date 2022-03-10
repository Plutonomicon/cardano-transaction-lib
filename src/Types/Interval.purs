-- | Adapted from `Plutus.V1.Ledger.Time`.
module Types.Interval
  ( Closure
  , Extended(..)
  , Interval(..)
  , LowerBound(..)
  , POSIXTime(..)
  , POSIXTimeRange
  , SlotConfig(..)
  , SlotRange
  , UpperBound(..)
  , after
  , always
  , before
  , beginningOfTime
  , contains
  , defaultSlotConfig
  , from
  , hull
  , intersection
  , interval
  , isEmpty
  , isEmpty'
  , lowerBound
  , member
  , mkInterval
  , never
  , overlaps
  , overlaps'
  , posixTimeRangeToContainedSlotRange
  , posixTimeRangeToTransactionSlot
  , posixTimeToEnclosingSlot
  , singleton
  , slotRangeToPOSIXTimeRange
  , slotRangeToTransactionSlot
  , slotToEndPOSIXTime
  , slotToPOSIXTimeRange
  , strictLowerBound
  , strictUpperBound
  , to
  , upperBound
  ) where

import Data.BigInt (BigInt, quot)
import Data.BigInt (fromString, fromInt, toString) as BigInt
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Data.UInt (fromString, toString) as UInt
import Partial.Unsafe (unsafePartial)
import Prelude
import Serialization.Address (Slot(Slot))

--------------------------------------------------------------------------------
-- Interval Type and related
--------------------------------------------------------------------------------
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
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

-- | An interval of `a`s.
-- |
-- | The interval may be either closed or open at either end, meaning
-- | that the endpoints may or may not be included in the interval.
-- |
-- | The interval can also be unbounded on either side.
newtype Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }

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
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Time.html#t:POSIXTimeRange
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
newtype POSIXTime = POSIXTime BigInt

derive instance Generic POSIXTime _
derive instance Newtype POSIXTime _
derive newtype instance Eq POSIXTime
derive newtype instance Ord POSIXTime
-- There isn't an Enum instance for BigInt so we derive Semiring instead which
-- has consequences on how isEmpty and overlaps are defined in
-- Types.POSIXTimeRange (Interval API).
derive newtype instance Semiring POSIXTime
instance Show POSIXTime where
  show = genericShow

-- | An `Interval` of `POSIXTime`s.
type POSIXTimeRange = Interval POSIXTime

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
mkInterval :: forall (a :: Type). LowerBound a -> UpperBound a -> Interval a
mkInterval ivFrom ivTo = Interval { ivFrom, ivTo }

strictUpperBound :: forall (a :: Type). a -> UpperBound a
strictUpperBound a = UpperBound (Finite a) false

strictLowerBound :: forall (a :: Type). a -> LowerBound a
strictLowerBound a = LowerBound (Finite a) false

lowerBound :: forall (a :: Type). a -> LowerBound a
lowerBound a = LowerBound (Finite a) true

upperBound :: forall (a :: Type). a -> UpperBound a
upperBound a = UpperBound (Finite a) true

-- | `interval a b` includes all values that are greater than or equal to `a`
-- | and smaller than or equal to `b`. Therefore it includes `a` and `b`.
interval :: forall (a :: Type). a -> a -> Interval a
interval s s' = mkInterval (lowerBound s) (upperBound s')

singleton :: forall (a :: Type). a -> Interval a
singleton s = interval s s

-- | `from a` is an `Interval` that includes all values that are
-- | greater than or equal to `a`.
from :: forall (a :: Type). a -> Interval a
from s = mkInterval (lowerBound s) (UpperBound PosInf true)

-- | `to a` is an `Interval` that includes all values that are
-- | smaller than or equal to `a`.
to :: forall (a :: Type). a -> Interval a
to s = mkInterval (LowerBound NegInf true) (upperBound s)

-- | An `Interval` that covers every slot.
always :: forall (a :: Type). Interval a
always = mkInterval (LowerBound NegInf true) (UpperBound PosInf true)

-- | An `Interval` that is empty.
never :: forall (a :: Type). Interval a
never = mkInterval (LowerBound PosInf true) (UpperBound NegInf true)

-- | Check whether a value is in an interval.
member :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
member a i = i `contains` singleton a

-- | Check whether two intervals overlap, that is, whether there is a value that
-- | is a member of both intervals. This is the Plutus implementation but
-- | `BigInt` used in `POSIXTime` is cannot be enumerated so the `isEmpty` we
-- | use in practice uses `Semiring` instead. See `overlaps` for the practical
-- | version.
overlaps'
  :: forall (a :: Type). Enum a => Interval a -> Interval a -> Boolean
overlaps' l r = not $ isEmpty' (l `intersection` r)

-- Potential FIX ME: shall we just fix the type to POSIXTime and remove overlaps'
-- and Semiring constraint?
-- | Check whether two intervals overlap, that is, whether there is a value that
-- | is a member of both intervals.
overlaps
  :: forall (a :: Type)
   . Ord a
  => Semiring a
  => Interval a
  -> Interval a
  -> Boolean
overlaps l r = not $ isEmpty (l `intersection` r)

-- | `intersection a b` is the largest interval that is contained in `a` and in
-- | `b`, if it exists.
intersection
  :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
intersection (Interval int) (Interval int') =
  mkInterval (max int.ivFrom int'.ivFrom) (min int.ivTo int'.ivTo)

-- | `hull a b` is the smallest interval containing `a` and `b`.
hull :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
hull (Interval int) (Interval int') =
  mkInterval (min int.ivFrom int'.ivFrom) (max int.ivTo int'.ivTo)

-- | `a` `contains` `b` is `true` if the `Interval b` is entirely contained in
-- | `a`. That is, `a `contains` `b` if for every entry `s`, if `member s b` then
-- | `member s a`.
contains :: forall (a :: Type). Ord a => Interval a -> Interval a -> Boolean
contains (Interval int) (Interval int') =
  int.ivFrom <= int'.ivFrom && int'.ivTo <= int.ivTo

-- | Check if an `Interval` is empty. This is the Plutus implementation but
-- | BigInt used in `POSIXTime` is cannot be enumerated so the `isEmpty` we use in
-- | practice uses `Semiring` instead. See `isEmpty` for the practical version.
isEmpty' :: forall (a :: Type). Enum a => Interval a -> Boolean
isEmpty' (Interval { ivFrom: LowerBound v1 in1, ivTo: UpperBound v2 in2 }) =
  case v1 `compare` v2 of
    LT -> if openInterval then checkEnds v1 v2 else false
    GT -> true
    EQ -> not (in1 && in2)
  where
  openInterval :: Boolean
  openInterval = not in1 && not in2

  -- | We check two finite ends to figure out if there are elements between them.
  -- | If there are no elements then the interval is empty.
  checkEnds :: Extended a -> Extended a -> Boolean
  checkEnds (Finite v1') (Finite v2') = (succ v1') `compare` (Just v2') == EQ
  checkEnds _ _ = false

-- Potential FIX ME: shall we just fix the type to POSIXTime and remove isEmpty'
-- and Semiring constraint?
-- | Check if an `Interval` is empty. This is the practical version to use
-- | with `a = POSIXTime`.
isEmpty :: forall (a :: Type). Ord a => Semiring a => Interval a -> Boolean
isEmpty (Interval { ivFrom: LowerBound v1 in1, ivTo: UpperBound v2 in2 }) =
  case v1 `compare` v2 of
    LT -> if openInterval then checkEnds v1 v2 else false
    GT -> true
    EQ -> not (in1 && in2)
  where
  openInterval :: Boolean
  openInterval = not in1 && not in2

  -- | We check two finite ends to figure out if there are elements between them.
  -- | If there are no elements then the interval is empty.
  checkEnds :: Extended a -> Extended a -> Boolean
  checkEnds (Finite v1') (Finite v2') = (v1' `add` one) `compare` v2' == EQ
  checkEnds _ _ = false

-- | Check if a value is earlier than the beginning of an `Interval`.
before :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
before h (Interval { ivFrom }) = lowerBound h < ivFrom

-- | Check if a value is later than the end of a `Interval`.
after :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
after h (Interval { ivTo }) = upperBound h > ivTo

--------------------------------------------------------------------------------
-- SlotConfig Type and related
--------------------------------------------------------------------------------
-- Most of these functions could use a Reader constraint over `SlotConfig` but
-- that would depend on how the `Contract` monad pans out, we'll keep it
-- explicit for now.

type SlotRange = Interval Slot

newtype SlotConfig = SlotConfig
  { slotLength :: BigInt -- Length (number of milliseconds) of one slot
  , slotZeroTime :: POSIXTime -- Beginning of slot 0 (in milliseconds)
  }

derive instance Generic SlotConfig _
derive newtype instance Eq SlotConfig

instance Show SlotConfig where
  show = genericShow

-- | 'beginningOfTime' corresponds to the Shelley launch date
-- | (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- | (number of milliseconds since 1970-01-01T00:00:00Z).
beginningOfTime :: BigInt
beginningOfTime = unsafePartial fromJust $ BigInt.fromString "1596059091000"

defaultSlotConfig :: SlotConfig
defaultSlotConfig = SlotConfig
  { slotLength: BigInt.fromInt 1000 -- One second = 1 slot currently.
  , slotZeroTime: POSIXTime beginningOfTime
  }

-- | Convert a `SlotRange` to a `POSIXTimeRange` given a `SlotConfig`. The
-- | resulting `POSIXTimeRange` refers to the starting time of the lower bound of
-- | the `SlotRange` and the ending time of the upper bound of the `SlotRange`.
slotRangeToPOSIXTimeRange :: SlotConfig -> SlotRange -> POSIXTimeRange
slotRangeToPOSIXTimeRange
  sc
  (Interval { ivFrom: LowerBound start startIncl, ivTo: UpperBound end endIncl }) =
  let
    lbound =
      map
        (if startIncl then slotToBeginPOSIXTime sc else slotToEndPOSIXTime sc)
        start
    ubound =
      map
        (if endIncl then slotToEndPOSIXTime sc else slotToBeginPOSIXTime sc)
        end
  in
    Interval
      { ivFrom: LowerBound lbound startIncl
      , ivTo: UpperBound ubound endIncl
      }

-- | Convert a `Slot` to a `POSIXTimeRange` given a `SlotConfig`. Each `Slot`
-- | can be represented by an interval of time.
slotToPOSIXTimeRange :: SlotConfig -> Slot -> POSIXTimeRange
slotToPOSIXTimeRange sc slot =
  interval (slotToBeginPOSIXTime sc slot) (slotToEndPOSIXTime sc slot)

-- UInt.toInt is unsafe so we'll go via String. BigInt.fromString returns a
-- Maybe but we should be safe if we go from UInt originally via String,
-- as this UInt can't be larger than BigInt.
uIntToBigInt :: UInt -> BigInt
uIntToBigInt = unsafePartial fromJust <<< BigInt.fromString <<< UInt.toString

-- This should be left allowed to fail as BigInt may exceed UInt
bigIntToUInt :: BigInt -> Maybe UInt
bigIntToUInt = UInt.fromString <<< BigInt.toString

-- | Get the starting `POSIXTime` of a `Slot` given a `SlotConfig`.
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime (SlotConfig { slotLength, slotZeroTime }) (Slot n) =
  let
    msAfterBegin = uIntToBigInt n * slotLength
  in
    POSIXTime $ unwrap slotZeroTime + msAfterBegin

-- | Get the ending `POSIXTime` of a `Slot` given a `SlotConfig`.
slotToEndPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToEndPOSIXTime sc@(SlotConfig { slotLength }) slot =
  slotToBeginPOSIXTime sc slot + POSIXTime (slotLength - one)

-- | Convert a `POSIXTimeRange` to `SlotRange` given a `SlotConfig`. This gives
-- | the biggest slot range that is entirely contained by the given time range.
posixTimeRangeToContainedSlotRange
  :: SlotConfig -> POSIXTimeRange -> Maybe SlotRange
posixTimeRangeToContainedSlotRange sc ptr = do
  let
    Interval
      { ivFrom: LowerBound start startIncl
      , ivTo: UpperBound end endIncl
      } = map (posixTimeToEnclosingSlot sc) ptr

    -- Determines the closure of the interval with a handler over whether it's
    -- the start or end of the interval and a default Boolean if we aren't
    -- dealing with the `Finite` case.
    closureWith
      :: (SlotConfig -> Slot -> POSIXTime)
      -> Boolean -- Default Boolean
      -> Extended Slot
      -> Boolean
    closureWith f def = case _ of
      Finite s -> f sc s `member` ptr
      _ -> def

    seqExtended :: Extended (Maybe Slot) -> Maybe (Extended Slot)
    seqExtended = case _ of
      Finite (Just s) -> pure $ Finite s
      Finite Nothing -> Nothing
      NegInf -> pure NegInf
      PosInf -> pure PosInf

  -- Fail if any of the outputs of `posixTimeToEnclosingSlot` are `Nothing`.
  start' <- seqExtended start
  end' <- seqExtended end
  pure $ Interval
    { ivFrom: LowerBound start' (closureWith slotToBeginPOSIXTime startIncl start')
    , ivTo: UpperBound end' (closureWith slotToBeginPOSIXTime endIncl end')
    }

type TransactionValiditySlot =
  { validityStartInterval :: Maybe Slot, timeToLive :: Maybe Slot }

-- | Converts a SlotRange to two separate slots used in building Types.Transaction.
-- | Note that we lose information regarding whether the bounds are included
-- | or not at `NegInf` and `PosInf`.
-- | `Nothing` for `validityStartInterval` represents `Slot zero`.
-- | `Nothing` for `timeToLive` represents `maxSlot`.
-- | For `Finite` values exclusive of bounds, we add and subtract one slot for
-- | `validityStartInterval` and `timeToLive`, respectively.
slotRangeToTransactionSlot
  :: SlotRange
  -> TransactionValiditySlot
slotRangeToTransactionSlot
  (Interval { ivFrom: LowerBound start startIncl, ivTo: UpperBound end endIncl }) =
  { validityStartInterval, timeToLive }
  where
  validityStartInterval :: Maybe Slot
  validityStartInterval = case start, startIncl of
    Finite s, true -> pure s
    Finite s, false -> pure $ s <> Slot one
    NegInf, _ -> Nothing
    PosInf, _ -> pure maxSlot

  timeToLive :: Maybe Slot
  timeToLive = case end, endIncl of
    Finite s, true -> pure s
    Finite s, false -> pure $ s <> Slot (negate one)
    NegInf, _ -> pure $ Slot zero
    PosInf, _ -> Nothing

-- | Maximum slot under `Data.UInt`
maxSlot :: Slot
maxSlot = Slot $ unsafePartial fromJust $ UInt.fromString "4294967295"

posixTimeRangeToTransactionSlot
  :: SlotConfig -> POSIXTimeRange -> Maybe TransactionValiditySlot
posixTimeRangeToTransactionSlot sc =
  map slotRangeToTransactionSlot <<< posixTimeRangeToContainedSlotRange sc

-- | Convert a `POSIXTime` to `Slot` given a `SlotConfig`. This differs from
-- | Plutus by potential failure.
posixTimeToEnclosingSlot :: SlotConfig -> POSIXTime -> Maybe Slot
posixTimeToEnclosingSlot (SlotConfig { slotLength, slotZeroTime }) (POSIXTime t) =
  let
    timePassed = t - unwrap slotZeroTime
    -- Plutus uses inbuilt `divide` which rounds towards downwards which is `quot`
    -- not `div`.
    slotsPassed = quot timePassed slotLength
  in
    Slot <$> bigIntToUInt slotsPassed

-- TO DO: https://github.com/Plutonomicon/cardano-browser-tx/issues/169
-- -- | Get the current slot number
-- currentSlot :: SlotConfig -> Effect Slot
