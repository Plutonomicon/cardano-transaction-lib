module Types.Interval
  ( AbsTime(..)
  , Closure
  , Extended(..)
  , Interval(..)
  , LowerBound(..)
  , ModTime(..)
  , OnchainPOSIXTimeRange(..)
  , POSIXTime(..)
  , POSIXTimeRange
  , PosixTimeToSlotError(..)
  , RelTime(..)
  , SlotRange
  , SlotToPosixTimeError(..)
  , ToOnChainPosixTimeRangeError(..)
  , UpperBound(..)
  , absSlotFromSlot
  , after
  , always
  , before
  , beginningOfTime
  , contains
  , findSlotEraSummary
  , findTimeEraSummary
  , from
  , hull
  , intersection
  , interval
  , isEmpty
  , isEmpty'
  , lowerBound
  , maxSlot
  , member
  , mkInterval
  , never
  , overlaps
  , overlaps'
  , posixTimeRangeToSlotRange
  , posixTimeRangeToTransactionValidity
  , posixTimeToSlot
  , singleton
  , slotFromAbsSlot
  , slotRangeToPosixTimeRange
  , slotToPosixTime
  , strictLowerBound
  , strictUpperBound
  , to
  , toOnchainPosixTimeRange
  , upperBound
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , encodeAeson'
  )
import Aeson.Decode ((</$\>), (</*\>))
import Aeson.Decode as D
import Aeson.Encode ((>$<), (>/\<))
import Aeson.Encode as E
import Control.Lazy (defer)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Data.Array (find)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber, fromString) as BigInt
import Data.Either (Either(Right), note)
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.JSDate (getTime, parse)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromString) as UInt
import Effect (Effect)
import Effect.Class (liftEffect)
import FromData (class FromData, genericFromData)
import Helpers (bigIntToUInt, liftEither, liftM, uIntToBigInt)
import Partial.Unsafe (unsafePartial)
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , SystemStart
  )
import Serialization.Address (Slot(Slot))
import ToData (class ToData, genericToData)
import TypeLevel.Nat (S, Z)

--------------------------------------------------------------------------------
-- Interval Type and related
--------------------------------------------------------------------------------
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- | Whether a bound is inclusive or not.
type Closure = Boolean

-- | A set extended with a positive and negative infinity.
data Extended a = NegInf | Finite a | PosInf

instance
  HasPlutusSchema
    (Extended a)
    ( "NegInf" := PNil @@ Z
        :+ "Finite"
        := PNil
        @@ (S Z)
        :+ "PosInf"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData a => ToData (Extended a) where
  toData = genericToData

instance FromData a => FromData (Extended a) where
  fromData = genericFromData

derive instance Generic (Extended a) _
derive instance Eq a => Eq (Extended a)
-- Don't change order of Extended of deriving Ord as below
derive instance Ord a => Ord (Extended a)
derive instance Functor Extended

instance Show a => Show (Extended a) where
  show = genericShow

-- | The lower bound of an interval.
data LowerBound a = LowerBound (Extended a) Closure

instance
  HasPlutusSchema (LowerBound a)
    ( "LowerBound" := PNil @@ Z
        :+ PNil
    )

instance ToData a => ToData (LowerBound a) where
  toData = genericToData

instance FromData a => FromData (LowerBound a) where
  fromData = genericFromData

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
data UpperBound :: Type -> Type
data UpperBound a = UpperBound (Extended a) Closure

instance
  HasPlutusSchema (UpperBound a)
    ( "UpperBound" := PNil @@ Z
        :+ PNil
    )

instance ToData a => ToData (UpperBound a) where
  toData = genericToData

instance FromData a => FromData (UpperBound a) where
  fromData = genericFromData

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
newtype Interval :: Type -> Type
newtype Interval a = Interval { from :: LowerBound a, to :: UpperBound a }

derive instance Generic (Interval a) _
derive newtype instance Eq a => Eq (Interval a)
derive instance Functor Interval
derive instance Newtype (Interval a) _

instance
  HasPlutusSchema (Interval a)
    ( "Interval"
        :=
          ( "from" := I (LowerBound a)
              :+ "to"
              := I (UpperBound a)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

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

instance ToData a => ToData (Interval a) where
  toData = genericToData

instance FromData a => FromData (Interval a) where
  fromData i = genericFromData i

instance EncodeAeson a => EncodeAeson (Interval a) where
  encodeAeson' (Interval i) = encodeAeson' $ HaskInterval
    { ivFrom: i.from, ivTo: i.to }

instance DecodeAeson a => DecodeAeson (Interval a) where
  decodeAeson a = do
    (HaskInterval i) <- decodeAeson a
    pure $ Interval { from: i.ivFrom, to: i.ivTo }

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
derive newtype instance FromData POSIXTime
derive newtype instance ToData POSIXTime

instance Show POSIXTime where
  show = genericShow

-- | An `Interval` of `POSIXTime`s.
type POSIXTimeRange = Interval POSIXTime

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
mkInterval :: forall (a :: Type). LowerBound a -> UpperBound a -> Interval a
mkInterval from' to' = Interval { from: from', to: to' }

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
  mkInterval (max int.from int'.from) (min int.to int'.to)

-- | `hull a b` is the smallest interval containing `a` and `b`.
hull :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
hull (Interval int) (Interval int') =
  mkInterval (min int.from int'.from) (max int.to int'.to)

-- | `a` `contains` `b` is `true` if the `Interval b` is entirely contained in
-- | `a`. That is, `a `contains` `b` if for every entry `s`, if `member s b` then
-- | `member s a`.
contains :: forall (a :: Type). Ord a => Interval a -> Interval a -> Boolean
contains (Interval int) (Interval int') =
  int.from <= int'.from && int'.to <= int.to

-- | Check if an `Interval` is empty. This is the Plutus implementation but
-- | BigInt used in `POSIXTime` is cannot be enumerated so the `isEmpty` we use in
-- | practice uses `Semiring` instead. See `isEmpty` for the practical version.
isEmpty' :: forall (a :: Type). Enum a => Interval a -> Boolean
isEmpty' (Interval { from: LowerBound v1 in1, to: UpperBound v2 in2 }) =
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
isEmpty (Interval { from: LowerBound v1 in1, to: UpperBound v2 in2 }) =
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
before h (Interval { from: from' }) = lowerBound h < from'

-- | Check if a value is later than the end of a `Interval`.
after :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
after h (Interval { to: to' }) = upperBound h > to'

-- | A newtype wrapper over `POSIXTimeRange` to represent the on-chain version
-- | of an off-chain `POSIXTimeRange`. In particular, there are a few steps
-- | in conversion:
-- | 1) `POSIXTimeRange` -> `SlotRange`
-- | 2) `SlotRange` -> `TransactionValidity`
-- | 3) `TransactionValidity` -> `OnchainPOSIXTimeRange`
-- | `OnchainPOSIXTimeRange` is intended to equal the validity range found in
-- | the on-chain `ScriptContext`
newtype OnchainPOSIXTimeRange = OnchainPOSIXTimeRange POSIXTimeRange

derive instance Generic OnchainPOSIXTimeRange _
derive instance Newtype OnchainPOSIXTimeRange _
derive newtype instance Eq OnchainPOSIXTimeRange
derive newtype instance JoinSemilattice OnchainPOSIXTimeRange
derive newtype instance BoundedJoinSemilattice OnchainPOSIXTimeRange
derive newtype instance MeetSemilattice OnchainPOSIXTimeRange
derive newtype instance BoundedMeetSemilattice OnchainPOSIXTimeRange
derive newtype instance FromData OnchainPOSIXTimeRange
derive newtype instance ToData OnchainPOSIXTimeRange

instance Show OnchainPOSIXTimeRange where
  show = genericShow

--------------------------------------------------------------------------------
-- SlotConfig Type and related
--------------------------------------------------------------------------------
-- Most of these functions could use a Reader constraint over `SlotConfig` but
-- that would depend on how the `Contract` monad pans out, we'll keep it
-- explicit for now.

type SlotRange = Interval Slot

-- | 'beginningOfTime' corresponds to the Shelley launch date
-- | (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- | (number of milliseconds since 1970-01-01T00:00:00Z).
beginningOfTime :: BigInt
beginningOfTime = unsafePartial fromJust $ BigInt.fromString "1596059091000"

-- | Maximum slot under `Data.UInt`
maxSlot :: Slot
maxSlot = Slot $ unsafePartial fromJust $ UInt.fromString "4294967295"

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Slot (absolute from System Start - see QueryM.SystemStart.getSystemStart)
-- to POSIXTime (milliseconds)
--------------------------------------------------------------------------------
data SlotToPosixTimeError
  = CannotFindSlotInEraSummaries AbsSlot
  | StartingSlotGreaterThanSlot AbsSlot
  | EndTimeLessThanTime AbsTime
  | CannotGetBigIntFromNumber

derive instance Generic SlotToPosixTimeError _
derive instance Eq SlotToPosixTimeError

instance Show SlotToPosixTimeError where
  show = genericShow

-- Based on:
-- https://github.com/input-output-hk/cardano-ledger/blob/2acff66e84d63a81de904e1c0de70208ff1819ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L186
-- https://github.com/input-output-hk/cardano-ledger/blob/1ec8b1428163dc36105b84735725414e1f4829be/eras/shelley/impl/src/Cardano/Ledger/Shelley/HardForks.hs
-- https://github.com/input-output-hk/cardano-base/blob/8fe904d629194b1fbaaf2d0a4e0ccd17052e9103/slotting/src/Cardano/Slotting/EpochInfo/API.hs#L80
-- https://input-output-hk.github.io/ouroboros-network/ouroboros-consensus/src/Ouroboros.Consensus.HardFork.History.EpochInfo.html
-- https://github.com/input-output-hk/ouroboros-network/blob/bd9e5653647c3489567e02789b0ec5b75c726db2/ouroboros-consensus/src/Ouroboros/Consensus/HardFork/History/Qry.hs#L461-L481
-- Could convert all errors to Strings and have Effect POSIXTime too:
-- | Converts a CSL (Absolute) `Slot` (Unsigned Integer) to `POSIXTime` which
-- | is time elapsed from January 1, 1970 (midnight UTC/GMT). We obtain this
-- | By converting `Slot` to `AbsTime` which is time relative to some System
-- | Start, then add any excess for a UNIX Epoch time. Recall that POSIXTime
-- | is in milliseconds for Protocol Version >= 6.
slotToPosixTime
  :: EraSummaries
  -> SystemStart
  -> Slot
  -> Effect (Either SlotToPosixTimeError POSIXTime)
slotToPosixTime eraSummaries sysStart slot = runExceptT do
  let absSlot = absSlotFromSlot slot
  -- Get JSDate:
  sysStartD <- liftEffect $ parse $ unwrap sysStart
  -- Find current era:
  currentEra <- liftEither $ findSlotEraSummary eraSummaries absSlot
  -- Convert absolute slot (relative to System start) to relative slot of era
  relSlot <- liftEither $ relSlotFromAbsSlot currentEra absSlot
  -- Convert relative slot to relative time for that era
  let relTime = relTimeFromRelSlot currentEra relSlot
  absTime <- liftEither $ absTimeFromRelTime currentEra relTime
  -- Get POSIX time for system start
  sysStartPosix <- liftM CannotGetBigIntFromNumber
    $ BigInt.fromNumber
    $ getTime sysStartD
  -- Add the system start time to the absolute time relative to system start
  -- to get overall POSIXTime
  pure $ wrap $ sysStartPosix + unwrap absTime
  where
  -- TODO: See https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley/impl/src/Cardano/Ledger/Shelley/HardForks.hs#L57
  -- translateTimeForPlutusScripts and ensure protocol version > 5 which would
  -- mean converting to milliseconds
  _transTime :: BigInt -> BigInt
  _transTime = (*) $ BigInt.fromInt 1000

-- | Convert a CSL (Absolute) `Slot` (`UInt`) to an Ogmios absolute slot
-- | (`BigInt`)
absSlotFromSlot :: Slot -> AbsSlot
absSlotFromSlot = wrap <<< uIntToBigInt <<< unwrap

-- | Convert an Ogmios absolute slot (`BigInt`) to a CSL (Absolute) `Slot`
-- | (`UInt`)
slotFromAbsSlot :: AbsSlot -> Maybe Slot
slotFromAbsSlot = map wrap <<< bigIntToUInt <<< unwrap

-- | Finds the `EraSummary` an `AbsSlot` lies inside (if any).
findSlotEraSummary
  :: EraSummaries
  -> AbsSlot -- Slot we are testing and trying to find inside `EraSummaries`
  -> Either SlotToPosixTimeError EraSummary
findSlotEraSummary (EraSummaries eraSummaries) os =
  note (CannotFindSlotInEraSummaries os) $ find pred eraSummaries
  where
  -- Potential FIXME: In the case of `Just`, do we want to use `safeZone` from
  -- `parameters` to provide a buffer?
  pred :: EraSummary -> Boolean
  pred (EraSummary { start, end }) =
    (unwrap start).slot <= os && maybe true ((<) os <<< _.slot <<< unwrap) end

-- | Relative slot of an `AbsSlot` within an `EraSummary`
newtype RelSlot = RelSlot BigInt

derive instance Generic RelSlot _
derive instance Newtype RelSlot _
derive newtype instance Eq RelSlot
derive newtype instance Ord RelSlot

instance Show RelSlot where
  show = genericShow

-- | Relative time to the start of an `EraSummary`. Contract this to
-- | `Ogmios.QueryM.RelativeTime` which is usually relative to system start.
-- | Treat as Milliseconds
newtype RelTime = RelTime BigInt

derive instance Generic RelTime _
derive instance Newtype RelTime _
derive newtype instance Eq RelTime
derive newtype instance Ord RelTime

instance Show RelTime where
  show = genericShow

-- | Any leftover time from using `mod` when dividing my slot length.
-- | Treat as Milliseconds
newtype ModTime = ModTime BigInt

derive instance Generic ModTime _
derive instance Newtype ModTime _
derive newtype instance Eq ModTime
derive newtype instance Ord ModTime

instance Show ModTime where
  show = genericShow

-- | Absolute time relative to System Start, not UNIX epoch.
-- | Treat as Milliseconds
newtype AbsTime = AbsTime BigInt

derive instance Generic AbsTime _
derive instance Newtype AbsTime _
derive newtype instance Eq AbsTime
derive newtype instance Ord AbsTime

instance Show AbsTime where
  show = genericShow

-- | Find the relative slot provided we know the `AbsSlot` for an absolute slot
-- | given an `EraSummary`. We could relax the `Either` monad if we use this
-- | in conjunction with `findSlotEraSummary`. However, we choose to make the
-- | function more general, guarding against a larger `start`ing slot
relSlotFromAbsSlot
  :: EraSummary -> AbsSlot -> Either SlotToPosixTimeError RelSlot
relSlotFromAbsSlot (EraSummary { start }) as@(AbsSlot absSlot) = do
  let startSlot = unwrap (unwrap start).slot
  unless (startSlot <= absSlot) (throwError $ StartingSlotGreaterThanSlot as)
  pure $ wrap $ absSlot - startSlot

relTimeFromRelSlot :: EraSummary -> RelSlot -> RelTime
relTimeFromRelSlot eraSummary (RelSlot relSlot) =
  let
    slotLength = getSlotLength eraSummary
  in
    wrap $ relSlot * slotLength

-- As justified in https://github.com/input-output-hk/ouroboros-network/blob/bd9e5653647c3489567e02789b0ec5b75c726db2/ouroboros-consensus/src/Ouroboros/Consensus/HardFork/History/Qry.hs#L461-L481
-- Treat the upperbound as inclusive.
-- | Returns the absolute time relative to some system start, not UNIX epoch.
absTimeFromRelTime
  :: EraSummary -> RelTime -> Either SlotToPosixTimeError AbsTime
absTimeFromRelTime (EraSummary { start, end }) (RelTime relTime) = do
  let
    startTime = unwrap (unwrap start).time * factor
    absTime = startTime + relTime -- relative to System Start, not UNIX Epoch.
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    -- Potential FIXME: note the hack that we don't have `end` for the current
    -- era, if we did not do this, there could be issues going far into the
    -- future?
    endTime = maybe (absTime + one)
      ((*) factor <<< unwrap <<< _.time <<< unwrap)
      end
  unless (absTime <= endTime) (throwError $ EndTimeLessThanTime $ wrap absTime)
  pure $ wrap absTime

--------------------------------------------------------------------------------
-- POSIXTime (milliseconds) to
-- Slot (absolute from System Start - see QueryM.SystemStart.getSystemStart)
--------------------------------------------------------------------------------
data PosixTimeToSlotError
  = CannotFindTimeInEraSummaries AbsTime
  | PosixTimeBeforeSystemStart POSIXTime
  | StartTimeGreaterThanTime AbsTime
  | EndSlotLessThanSlotOrModNonZero AbsSlot ModTime
  | CannotConvertAbsSlotToSlot AbsSlot
  | CannotGetBigIntFromNumber'

derive instance Generic PosixTimeToSlotError _
derive instance Eq PosixTimeToSlotError

instance Show PosixTimeToSlotError where
  show = genericShow

-- | Converts a `POSIXTime` to `Slot` given an `EraSummaries` and
-- | `SystemStart` queried from Ogmios.
posixTimeToSlot
  :: EraSummaries
  -> SystemStart
  -> POSIXTime
  -> Effect (Either PosixTimeToSlotError Slot)
posixTimeToSlot eraSummaries sysStart pt'@(POSIXTime pt) = runExceptT do
  -- Get JSDate:
  sysStartD <- liftEffect $ parse $ unwrap sysStart
  -- Get POSIX time for system start
  sysStartPosix <- liftM CannotGetBigIntFromNumber'
    $ BigInt.fromNumber
    $ getTime sysStartD
  -- Ensure the time we are converting is after the system start, otherwise
  -- we have negative slots.
  unless (sysStartPosix <= pt)
    $ throwError
    $ PosixTimeBeforeSystemStart pt'
  -- Keep as milliseconds:
  let absTime = wrap $ pt - sysStartPosix
  -- Find current era:
  currentEra <- liftEither $ findTimeEraSummary eraSummaries absTime
  -- Get relative time from absolute time w.r.t. current era
  relTime <- liftEither $ relTimeFromAbsTime currentEra absTime
  -- Convert to relative slot
  let relSlotMod = relSlotFromRelTime currentEra relTime
  -- Get absolute slot relative to system start
  absSlot <- liftEither $ absSlotFromRelSlot currentEra relSlotMod
  -- Convert back to UInt `Slot`
  liftM (CannotConvertAbsSlotToSlot absSlot) $ slotFromAbsSlot absSlot

-- | Finds the `EraSummary` an `AbsTime` lies inside (if any).
findTimeEraSummary
  :: EraSummaries
  -> AbsTime -- Time we are testing and trying to find inside `EraSummaries`
  -> Either PosixTimeToSlotError EraSummary
findTimeEraSummary (EraSummaries eraSummaries) absTime@(AbsTime at) =
  note (CannotFindTimeInEraSummaries absTime) $ find pred eraSummaries
  where
  pred :: EraSummary -> Boolean
  pred (EraSummary { start, end }) =
    unwrap (unwrap start).time * factor <= at
      && maybe true ((<) at <<< (*) factor <<< unwrap <<< _.time <<< unwrap) end

-- Use this factor to convert Ogmios seconds to Milliseconds for example, I
-- think this is safe e.g. see https://cardano.stackexchange.com/questions/7034/how-to-convert-posixtime-to-slot-number-on-cardano-testnet/7035#7035
-- that indeed, start of eras should be exact to the second.
factor :: BigInt
factor = BigInt.fromInt 1000

relTimeFromAbsTime
  :: EraSummary -> AbsTime -> Either PosixTimeToSlotError RelTime
relTimeFromAbsTime (EraSummary { start }) at@(AbsTime absTime) = do
  let startTime = unwrap (unwrap start).time * factor
  unless (startTime <= absTime) (throwError $ StartTimeGreaterThanTime at)
  let relTime = absTime - startTime -- relative to era start, not UNIX Epoch.
  pure $ wrap relTime

-- | Converts relative time to relative slot (using Euclidean division) and
-- | modulus for any leftover.
relSlotFromRelTime
  :: EraSummary -> RelTime -> RelSlot /\ ModTime
relSlotFromRelTime eraSummary (RelTime relTime) =
  let
    slotLength = getSlotLength eraSummary
  in
    wrap (relTime `div` slotLength) /\ wrap (relTime `mod` slotLength) -- Euclidean division okay as everything is non-negative

absSlotFromRelSlot
  :: EraSummary -> RelSlot /\ ModTime -> Either PosixTimeToSlotError AbsSlot
absSlotFromRelSlot
  (EraSummary { start, end })
  (RelSlot relSlot /\ mt@(ModTime modTime)) = do
  let
    startSlot = unwrap (unwrap start).slot
    -- Round down to the nearest Slot to accept Milliseconds as input.
    absSlot = startSlot + relSlot -- relative to system start
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    -- Potential FIXME: note the hack that we don't have `end` for the current
    -- era, if we did not do this, there could be issues going far into the
    -- future?
    endSlot = maybe (absSlot + one) (unwrap <<< _.slot <<< unwrap) end
  -- Check we are less than the end slot, or if equal, there is no excess:
  unless (absSlot < endSlot || absSlot == endSlot && modTime == zero)
    (throwError $ EndSlotLessThanSlotOrModNonZero (wrap absSlot) mt)
  -- Potential FIXME: Do we want to use `safeZone` from `parameters`?
  pure $ wrap absSlot

-- | Get SlotLength in Milliseconds
getSlotLength :: EraSummary -> BigInt
getSlotLength (EraSummary { parameters }) =
  unwrap (unwrap parameters).slotLength * factor

--------------------------------------------------------------------------------

-- | Converts a `POSIXTimeRange` to `SlotRange` given an `EraSummaries` and
-- | `SystemStart` queried from Ogmios.
posixTimeRangeToSlotRange
  :: EraSummaries
  -> SystemStart
  -> POSIXTimeRange
  -> Effect (Either PosixTimeToSlotError SlotRange)
posixTimeRangeToSlotRange
  eraSummaries
  sysStart
  (Interval { from: LowerBound s sInc, to: UpperBound e endInc }) = runExceptT
  do
    s' <- ExceptT $ convertBounds s
    e' <- ExceptT $ convertBounds e
    liftEither $ Right
      $ Interval { from: LowerBound s' sInc, to: UpperBound e' endInc }
  where
  convertBounds
    :: Extended POSIXTime
    -> Effect (Either PosixTimeToSlotError (Extended Slot))
  convertBounds (Finite pt) = posixTimeToSlot eraSummaries sysStart pt
    <#> map Finite
  convertBounds NegInf = pure $ Right NegInf
  convertBounds PosInf = pure $ Right PosInf

-- | Converts a `SlotRange` to `POSIXTimeRange` given an `EraSummaries` and
-- | `SystemStart` queried from Ogmios.
slotRangeToPosixTimeRange
  :: EraSummaries
  -> SystemStart
  -> SlotRange
  -> Effect (Either SlotToPosixTimeError POSIXTimeRange)
slotRangeToPosixTimeRange
  eraSummaries
  sysStart
  (Interval { from: LowerBound s sInc, to: UpperBound e endInc }) = runExceptT
  do
    s' <- ExceptT $ convertBounds s
    e' <- ExceptT $ convertBounds e
    liftEither $ Right
      $ Interval { from: LowerBound s' sInc, to: UpperBound e' endInc }
  where
  convertBounds
    :: Extended Slot
    -> Effect (Either SlotToPosixTimeError (Extended POSIXTime))
  convertBounds (Finite pt) = slotToPosixTime eraSummaries sysStart pt
    <#> map Finite
  convertBounds NegInf = pure $ Right NegInf
  convertBounds PosInf = pure $ Right PosInf

type TransactionValiditySlot =
  { validityStartInterval :: Maybe Slot, timeToLive :: Maybe Slot }

-- | Converts a `SlotRange` to two separate slots used in building
-- | Cardano.Types.Transaction.
-- | Note that we lose information regarding whether the bounds are included
-- | or not at `NegInf` and `PosInf`.
-- | `Nothing` for `validityStartInterval` represents `Slot zero`.
-- | `Nothing` for `timeToLive` represents `maxSlot`.
-- | For `Finite` values exclusive of bounds, we add and subtract one slot for
-- | `validityStartInterval` and `timeToLive`, respectively
slotRangeToTransactionValidity
  :: SlotRange
  -> TransactionValiditySlot
slotRangeToTransactionValidity
  (Interval { from: LowerBound start startInc, to: UpperBound end endInc }) =
  { validityStartInterval, timeToLive }
  where
  -- https://github.com/input-output-hk/cardano-ledger/blob/94b1ae3d6b66f4232f34060d89c2f12628998ef2/eras/shelley-ma/impl/src/Cardano/Ledger/ShelleyMA/Timelocks.hs#L100-L106
  -- The lower bound should be closed, so we add one for open bounds.
  validityStartInterval :: Maybe Slot
  validityStartInterval = case start, startInc of
    Finite s, true -> pure s
    Finite s, false -> pure $ s <> Slot one
    NegInf, _ -> Nothing
    PosInf, _ -> pure maxSlot

  -- https://github.com/input-output-hk/cardano-ledger/blob/94b1ae3d6b66f4232f34060d89c2f12628998ef2/eras/shelley-ma/impl/src/Cardano/Ledger/ShelleyMA/Timelocks.hs#L100-L106
  -- in accordance to the above, the upper bound is open. So we should add one
  -- for closed upper bounds.
  timeToLive :: Maybe Slot
  timeToLive = case end, endInc of
    Finite s, true -> pure $ s <> Slot one
    Finite s, false -> pure s
    NegInf, _ -> pure $ Slot zero
    PosInf, _ -> Nothing

-- | Converts a `POSIXTimeRange` to a transaction validity interval via a
-- | `SlotRange` to be used when building a CSL transaction body
posixTimeRangeToTransactionValidity
  :: EraSummaries
  -> SystemStart
  -> POSIXTimeRange
  -> Effect (Either PosixTimeToSlotError TransactionValiditySlot)
posixTimeRangeToTransactionValidity es ss =
  map (map slotRangeToTransactionValidity) <<< posixTimeRangeToSlotRange es ss

data ToOnChainPosixTimeRangeError
  = PosixTimeToSlotError' PosixTimeToSlotError
  | SlotToPosixTimeError' SlotToPosixTimeError

derive instance Generic ToOnChainPosixTimeRangeError _
derive instance Eq ToOnChainPosixTimeRangeError

instance Show ToOnChainPosixTimeRangeError where
  show = genericShow

-- TO DO: https://github.com/Plutonomicon/cardano-transaction-lib/issues/169
-- -- | Get the current slot number
-- currentSlot :: SlotConfig -> Effect Slot

-- NOTE: mlabs-haskell/purescript-bridge generated and applied here

newtype HaskInterval a = HaskInterval
  { ivFrom :: LowerBound a, ivTo :: UpperBound a }

derive instance Generic (HaskInterval a) _
derive newtype instance Eq a => Eq (HaskInterval a)
derive instance Functor HaskInterval
derive instance Newtype (HaskInterval a) _

instance (EncodeAeson a) => EncodeAeson (HaskInterval a) where
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> E.encode $ unwrap >$<
        ( E.record
            { ivFrom: E.value :: _ (LowerBound a)
            , ivTo: E.value :: _ (UpperBound a)
            }
        )
    ) x

instance (DecodeAeson a) => DecodeAeson (HaskInterval a) where
  decodeAeson = defer \_ -> D.decode $
    ( HaskInterval <$> D.record "Interval"
        { ivFrom: D.value :: _ (LowerBound a)
        , ivTo: D.value :: _ (UpperBound a)
        }
    )

instance (EncodeAeson a) => EncodeAeson (LowerBound a) where
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> E.encode $ (case _ of LowerBound a b -> (a /\ b)) >$<
        (E.tuple (E.value >/\< E.value))
    ) x

instance (DecodeAeson a) => DecodeAeson (LowerBound a) where
  decodeAeson = defer \_ -> D.decode $
    (D.tuple $ LowerBound </$\> D.value </*\> D.value)

instance (EncodeAeson a) => EncodeAeson (UpperBound a) where
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> E.encode $ (case _ of UpperBound a b -> (a /\ b)) >$<
        (E.tuple (E.value >/\< E.value))
    ) x

instance (DecodeAeson a) => DecodeAeson (UpperBound a) where
  decodeAeson = defer \_ -> D.decode $
    (D.tuple $ UpperBound </$\> D.value </*\> D.value)

instance (EncodeAeson a) => EncodeAeson (Extended a) where
  encodeAeson' x = encodeAeson' $
    ( defer \_ -> case _ of
        NegInf -> encodeAeson { tag: "NegInf" }
        Finite a -> E.encodeTagged "Finite" a E.value
        PosInf -> encodeAeson { tag: "PosInf" }
    ) x

instance (DecodeAeson a) => DecodeAeson (Extended a) where
  decodeAeson = defer \_ -> D.decode
    $ D.sumType "Extended"
    $ Map.fromFoldable
        [ "NegInf" /\ pure NegInf
        , "Finite" /\ D.content (Finite <$> D.value)
        , "PosInf" /\ pure PosInf
        ]

-- https://github.com/input-output-hk/cardano-ledger/blob/2acff66e84d63a81de904e1c0de70208ff1819ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L206-L226
-- | Create an `OnchainPOSIXTimeRange` to do a round trip from an off-chain
-- | POSIXTimeRange as follows:
-- | 1) `POSIXTimeRange` -> `SlotRange`
-- | 2) `SlotRange` -> `TransactionValidity`
-- | 3) `TransactionValidity` -> `OnchainPOSIXTimeRange`
-- | `OnchainPOSIXTimeRange` is intended to equal the validity range found in
-- | the on-chain `ScriptContext`
toOnchainPosixTimeRange
  :: EraSummaries
  -> SystemStart
  -> POSIXTimeRange
  -> Effect (Either ToOnChainPosixTimeRangeError OnchainPOSIXTimeRange)
toOnchainPosixTimeRange es ss ptr = runExceptT do
  { validityStartInterval, timeToLive } <-
    ExceptT $ posixTimeRangeToTransactionValidity es ss ptr
      <#> lmap PosixTimeToSlotError'
  case validityStartInterval, timeToLive of
    Nothing, Nothing -> liftEither $ Right $ wrap always
    Just s, Nothing -> ExceptT $ slotToPosixTime es ss s
      <#> bimap SlotToPosixTimeError' (from >>> wrap)
    Nothing, Just s -> ExceptT $ slotToPosixTime es ss s
      <#> bimap SlotToPosixTimeError' (to >>> wrap)
    Just s1, Just s2 -> do
      t1 <- ExceptT $ slotToPosixTime es ss s1 <#> lmap SlotToPosixTimeError'
      t2 <- ExceptT $ slotToPosixTime es ss s2 <#> lmap SlotToPosixTimeError'
      liftEither $ Right $ wrap $ interval t1 t2
