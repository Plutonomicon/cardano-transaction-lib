module Ctl.Internal.Types.Interval
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
  , RelSlot(..)
  , RelTime(..)
  , SlotRange
  , SlotToPosixTimeError(..)
  , ToOnChainPosixTimeRangeError(..)
  , UpperBound(..)
  , after
  , always
  , before
  , beginningOfTime
  , contains
  , findSlotEraSummary
  , findTimeEraSummary
  , from
  , genFiniteInterval
  , genLowerRay
  , genUpperRay
  , getSlotLength
  , highestEndSlotInEraSummaries
  , hull
  , intersection
  , isEmpty
  , isEmpty'
  , lowerBound
  , maxSlot
  , member
  , mkFiniteInterval
  , never
  , overlaps
  , overlaps'
  , posixTimeRangeToSlotRange
  , posixTimeRangeToTransactionValidity
  , posixTimeToSlot
  , singleton
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
  , Aeson
  , JsonDecodeError(AtKey, Named, TypeMismatch, UnexpectedValue)
  , aesonNull
  , decodeAeson
  , encodeAeson
  , getField
  , isNull
  , partialFiniteNumber
  , (.:)
  )
import Contract.Prelude (maximum)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Ctl.Internal.FromData (class FromData, fromData, genericFromData)
import Ctl.Internal.Helpers
  ( contentsProp
  , encodeTagged'
  , liftEither
  , liftM
  , mkErrorRecord
  , showWithParens
  , tagProp
  )
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  )
import Ctl.Internal.QueryM.Ogmios (aesonObject, slotLengthFactor)
import Ctl.Internal.Serialization.Address (Slot(..))
import Ctl.Internal.ToData (class ToData, genericToData, toData)
import Ctl.Internal.TypeLevel.Nat (S, Z)
import Ctl.Internal.Types.BigNum
  ( add
  , fromBigInt
  , maxValue
  , one
  , toBigInt
  , zero
  ) as BigNum
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  )
import Ctl.Internal.Types.PlutusData (PlutusData(Constr))
import Ctl.Internal.Types.SystemStart (SystemStart, sysStartUnixTime)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (find, head, index, length)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber, fromString, toNumber) as BigInt
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Foreign.Object (Object)
import Math (trunc, (%)) as Math
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, frequency)

--------------------------------------------------------------------------------
-- Interval Type and related
--------------------------------------------------------------------------------
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- | Whether a bound is inclusive or not.
type Closure = Boolean

-- | A set extended with a positive and negative infinity.
data Extended :: Type -> Type
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

-- | An abstraction for `Interval`,
-- | it restricts the creation of intervals
-- | to the only ones that we can use in practice:
-- | [a,b+1) , [-infinity,b+1), [a,infinity], [-infinity,infinity],
-- | empty.
-- see https://github.com/Plutonomicon/cardano-transaction-lib/issues/1041
-- In some functions we ask for a `Ord` and `Semiring` or `Ring` constraint
-- Those two constraints are require to be compatible
-- by example we expect that  `(x < x + 1)== true`
data Interval :: Type -> Type
data Interval a
  = FiniteInterval a a
  | StartAt a
  | EndAt a
  | AlwaysInterval
  | EmptyInterval

derive instance Generic (Interval a) _
derive instance Eq a => Eq (Interval a)
derive instance Functor Interval

instance (Show a, Ord a, Semiring a) => Show (Interval a) where
  show (FiniteInterval start end) = "(FiniteInterval " <> show start <> " "
    <> show end
    <> ")"
  show (StartAt end) = "(StartAt " <> show end <> ")"
  show (EndAt start) = "(EndAt " <> show start <> ")"
  show AlwaysInterval = "AlwaysInterval"
  show EmptyInterval = "EmptyInterval"

instance Ord a => MeetSemilattice (Interval a) where
  meet = intersection

-- This instance is written to be compatible with plutus.
instance (ToData a, Ord a, Semiring a) => ToData (Interval a) where
  toData (FiniteInterval start end) =
    ( Constr BigNum.zero
        [ toData $ lowerBound start
        , toData $ strictUpperBound (end + one)
        ]
    )
  toData (StartAt end) =
    ( Constr BigNum.zero
        [ toData (LowerBound NegInf true :: LowerBound a)
        , toData $ strictUpperBound (end + one)
        ]
    )
  toData (EndAt start) =
    ( Constr BigNum.zero
        [ toData $ lowerBound start
        , toData (UpperBound PosInf true :: UpperBound a)
        ]
    )
  toData AlwaysInterval =
    ( Constr BigNum.zero
        [ toData (LowerBound NegInf true :: LowerBound a)
        , toData (UpperBound PosInf true :: UpperBound a)
        ]
    )
  toData EmptyInterval =
    ( Constr BigNum.zero
        [ toData (LowerBound PosInf true :: LowerBound a)
        , toData (UpperBound NegInf true :: UpperBound a)
        ]
    )

instance Ord a => JoinSemilattice (Interval a) where
  join = hull

instance Ord a => BoundedMeetSemilattice (Interval a) where
  top = AlwaysInterval

instance Ord a => BoundedJoinSemilattice (Interval a) where
  bottom = EmptyInterval

-- This instance is written to be compatible with plutus.
instance (FromData a, Ord a, Ring a) => FromData (Interval a) where
  fromData (Constr index [ lower, upper ]) | index == BigNum.zero = do
    (LowerBound start startBool) <- fromData lower
    (UpperBound end endBool) <- fromData upper
    case
      add (if startBool then zero else one) <$> start,
      flip sub (if endBool then zero else one) <$> end
      of
      Finite start', Finite end' ->
        pure $ mkFiniteInterval start' end'
      Finite _, NegInf ->
        pure never
      Finite start', PosInf ->
        pure $ from start'
      NegInf, Finite end' ->
        pure $ to end'
      NegInf, NegInf ->
        pure never
      NegInf, PosInf ->
        pure always
      PosInf, _ ->
        pure never
  fromData _ = Nothing

instance (EncodeAeson a, Ord a, Semiring a) => EncodeAeson (Interval a) where
  encodeAeson = encodeAeson <<< intervalToHaskInterval

instance (DecodeAeson a, Ord a, Ring a) => DecodeAeson (Interval a) where
  decodeAeson a = do
    haskInterval <- decodeAeson a
    pure $ haskIntervalToInterval haskInterval

instance (Arbitrary a, Ord a, Semiring a) => Arbitrary (Interval a) where
  arbitrary = frequency $ wrap $
    (0.25 /\ genFiniteInterval arbitrary)
      :| (0.25 /\ genUpperRay arbitrary)
        : (0.25 /\ genLowerRay arbitrary)
        : (0.1 /\ genSingletonInterval)
        : (0.075 /\ pure always)
        : (0.075 /\ pure never)
        : Nil

-- | those accept a generator since we want to use them
-- | for Positive Integers in tests
genFiniteInterval
  :: forall (a :: Type). Ord a => Gen a -> Gen (Interval a)
genFiniteInterval generator = do
  in1 <- generator
  in2 <- generator
  let
    start = min in1 in2
    end = max in1 in2
  pure $ mkFiniteInterval start end

genUpperRay
  :: forall (a :: Type). Ord a => Gen a -> Gen (Interval a)
genUpperRay generator = from <$> generator

genLowerRay
  :: forall (a :: Type). Ord a => Gen a -> Gen (Interval a)
genLowerRay generator = to <$> generator

genSingletonInterval
  :: forall (a :: Type). Ord a => Arbitrary a => Semiring a => Gen (Interval a)
genSingletonInterval = singleton <$> arbitrary

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
derive newtype instance Semiring POSIXTime
derive newtype instance Ring POSIXTime
derive newtype instance FromData POSIXTime
derive newtype instance ToData POSIXTime
derive newtype instance DecodeAeson POSIXTime
derive newtype instance EncodeAeson POSIXTime

instance Show POSIXTime where
  show (POSIXTime pt) = showWithParens "POSIXTime" pt

-- | An `Interval` of `POSIXTime`s.
type POSIXTimeRange = Interval POSIXTime

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
strictUpperBound :: forall (a :: Type). a -> UpperBound a
strictUpperBound a = UpperBound (Finite a) false

strictLowerBound :: forall (a :: Type). a -> LowerBound a
strictLowerBound a = LowerBound (Finite a) false

lowerBound :: forall (a :: Type). a -> LowerBound a
lowerBound a = LowerBound (Finite a) true

upperBound :: forall (a :: Type). a -> UpperBound a
upperBound a = UpperBound (Finite a) true

-- | `mkFiniteInterval a b` construct the interval [a,b+1)
mkFiniteInterval :: forall (a :: Type). Ord a => a -> a -> Interval a
mkFiniteInterval x y = if x <= y then FiniteInterval x y else EmptyInterval

-- | `singleton a` is an `Interval` that contains just `a`,
-- | represented as [a,a+1).
singleton :: forall (a :: Type). Ord a => Semiring a => a -> Interval a
singleton s = mkFiniteInterval s s

-- | `from a` is an `Interval` that includes all values that are
-- | greater than or equal to `a`, represented as [a,infinity].
from :: forall (a :: Type). Ord a => a -> Interval a
from s = EndAt s

-- | `to a` is an `Interval` that includes all values that are
-- | smaller than or equal to `a`, represented as [-infinity,a+1).
to :: forall (a :: Type). Ord a => a -> Interval a
to s = StartAt s

-- | An `Interval` that covers every slot, represented
-- | as [-infinity,infinity]
always :: forall (a :: Type). Ord a => Interval a
always = AlwaysInterval

-- | An `Interval` that is empty, represented as
-- | [infinity,-infinity]
never :: forall (a :: Type). Ord a => Interval a
never = EmptyInterval

-- | Check whether a value is in an interval.
member :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
member a (FiniteInterval start end) = start <= a && a <= end
member a (StartAt end) = a <= end
member a (EndAt start) = start <= a
member _ AlwaysInterval = true
member _ EmptyInterval = false

-- | Check whether two intervals overlap, that is, whether there is a value that
-- | is a member of both intervals.
overlaps'
  :: forall (a :: Type)
   . Warn (Text "Deprecated, use `overlaps` instead")
  => Ord a
  => Interval a
  -> Interval a
  -> Boolean
overlaps' = overlaps

-- | Check whether two intervals overlap, that is, whether there is a value that
-- | is a member of both intervals.
overlaps
  :: forall (a :: Type)
   . Ord a
  => Interval a
  -> Interval a
  -> Boolean
overlaps l r = not $ isEmpty (l `intersection` r)

-- | `intersection a b` is the largest interval that is contained in `a` and in
-- | `b`, if it exists.
intersection
  :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
intersection (FiniteInterval start end) (FiniteInterval start' end') =
  mkFiniteInterval (max start start') (min end end')
intersection (FiniteInterval start end) (StartAt end') =
  mkFiniteInterval start (min end end')
intersection (FiniteInterval start end) (EndAt start') =
  mkFiniteInterval (max start start') end
intersection (StartAt end) (StartAt end') =
  StartAt (min end end')
intersection (StartAt end) (EndAt start') =
  mkFiniteInterval start' end
intersection (EndAt start) (EndAt start') =
  EndAt (max start start')
intersection AlwaysInterval i = i
intersection EmptyInterval _ = EmptyInterval
intersection i1 i2 = intersection i2 i1

-- | `hull a b` is the smallest interval containing `a` and `b` between our
-- | limited universe of intervals.
hull :: forall (a :: Type). Ord a => Interval a -> Interval a -> Interval a
hull (FiniteInterval start end) (FiniteInterval start' end') =
  mkFiniteInterval (min start start') (max end end')
hull (FiniteInterval _ end) (StartAt end') =
  StartAt (max end end')
hull (FiniteInterval start _) (EndAt start') =
  EndAt (min start start')
hull (StartAt end) (StartAt end') =
  StartAt (max end end')
hull (StartAt _) (EndAt _) = AlwaysInterval
hull (EndAt start) (EndAt start') =
  EndAt (min start start')
hull _ AlwaysInterval = AlwaysInterval
hull i EmptyInterval = i
hull i1 i2 = hull i2 i1

-- | `a` `contains` `b` is `true` if the `Interval b` is entirely contained in
-- | `a`. That is, `a `contains` `b` if for every entry `s`, if `member s b` then
-- | `member s a`.
contains :: forall (a :: Type). Ord a => Interval a -> Interval a -> Boolean
contains (FiniteInterval start end) (FiniteInterval start' end') =
  start <= start' && end' <= end
contains (FiniteInterval _ _) EmptyInterval = true
contains (FiniteInterval _ _) _ = false
contains (StartAt end) (FiniteInterval _' end') = end' <= end
contains (StartAt end) (StartAt end') = end' <= end
contains (StartAt _) EmptyInterval = true
contains (StartAt _) _ = false
contains (EndAt start) (FiniteInterval start' _) = start <= start'
contains (EndAt start) (EndAt start') = start <= start'
contains (EndAt _) EmptyInterval = true
contains (EndAt _) _ = false
contains AlwaysInterval _ = true
contains EmptyInterval EmptyInterval = true
contains EmptyInterval _ = false

-- | Check if an `Interval` is empty.
isEmpty :: forall (a :: Type). Ord a => Interval a -> Boolean
isEmpty (FiniteInterval start end) = end < start
isEmpty EmptyInterval = true
isEmpty _ = false

isEmpty'
  :: forall (a :: Type)
   . Ord a
  => Warn (Text "Deprecated, use `isEmpty` instead")
  => Interval a
  -> Boolean
isEmpty' = isEmpty

-- | Check if a value is earlier than the beginning of an `Interval`.
before :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
before h (FiniteInterval start _) = h < start
before h (EndAt start) = h < start
before _ EmptyInterval = true
before _ _ = false

-- | Check if a value is later than the end of a `Interval`.
after :: forall (a :: Type). Ord a => a -> Interval a -> Boolean
after h (FiniteInterval _ end) = end < h
after h (StartAt end) = end < h
after _ EmptyInterval = true
after _ _ = false

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
beginningOfTime =
  unsafePartial fromJust $
    BigInt.fromString "1596059091000"

-- | Maximum slot (u64)
maxSlot :: Slot
maxSlot = wrap BigNum.maxValue

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Slot (absolute from System Start - see QueryM.SystemStart.getSystemStart)
-- to POSIXTime (milliseconds)
--------------------------------------------------------------------------------
data SlotToPosixTimeError
  = CannotFindSlotInEraSummaries Slot
  | StartingSlotGreaterThanSlot Slot
  | EndTimeLessThanTime Number
  | CannotGetBigIntFromNumber

derive instance Generic SlotToPosixTimeError _
derive instance Eq SlotToPosixTimeError

instance Show SlotToPosixTimeError where
  show = genericShow

slotToPosixTimeErrorStr :: String
slotToPosixTimeErrorStr = "slotToPosixTimeError"

instance EncodeAeson SlotToPosixTimeError where
  encodeAeson (CannotFindSlotInEraSummaries slot) =
    encodeAeson $ mkErrorRecord
      slotToPosixTimeErrorStr
      "cannotFindSlotInEraSummaries"
      [ slot ]
  encodeAeson (StartingSlotGreaterThanSlot slot) = do
    encodeAeson $ mkErrorRecord
      slotToPosixTimeErrorStr
      "startingSlotGreaterThanSlot"
      [ slot ]
  encodeAeson (EndTimeLessThanTime absTime) = do
    encodeAeson $ mkErrorRecord
      slotToPosixTimeErrorStr
      "endTimeLessThanTime"
      -- We assume the numbers are finite
      [ unsafePartial partialFiniteNumber absTime ]
  encodeAeson CannotGetBigIntFromNumber = do
    encodeAeson $ mkErrorRecord
      slotToPosixTimeErrorStr
      "cannotGetBigIntFromNumber"
      aesonNull

instance DecodeAeson SlotToPosixTimeError where
  decodeAeson = aesonObject $ \o -> do
    errorType <- getField o "errorType"
    unless (errorType == slotToPosixTimeErrorStr)
      $ throwError
      $ TypeMismatch "Expected SlotToPosixTimeError"
    getField o "error" >>= case _ of
      "cannotFindSlotInEraSummaries" -> do
        arg <- extractArg o
        pure $ CannotFindSlotInEraSummaries arg
      "startingSlotGreaterThanSlot" -> do
        arg <- extractArg o
        pure $ StartingSlotGreaterThanSlot arg
      "endTimeLessThanTime" -> do
        arg <- extractArg o
        pure $ EndTimeLessThanTime arg
      "cannotGetBigIntFromNumber" -> do
        args <- getField o "args"
        unless (isNull args) (throwError $ TypeMismatch "Non-empty args")
        pure CannotGetBigIntFromNumber
      _ -> throwError $ TypeMismatch "Unknown error message"

-- Extracts a singleton array from an object for "args"
extractArg
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> Either JsonDecodeError a
extractArg o = do
  args <- getField o "args"
  when (length args /= one) (throwError $ TypeMismatch "Incorrect args")
  note (TypeMismatch "Could not extract head") (head args)

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
  -- Find current era:
  currentEra <- liftEither $ findSlotEraSummary eraSummaries slot
  -- Convert absolute slot (relative to System start) to relative slot of era
  relSlot <- liftEither $ relSlotFromSlot currentEra slot
  -- Convert relative slot to relative time for that era
  relTime <- liftM CannotGetBigIntFromNumber $ relTimeFromRelSlot currentEra
    relSlot
  absTime <- liftEither $ absTimeFromRelTime currentEra relTime
  -- Get POSIX time for system start
  sysStartPosix <- liftM CannotGetBigIntFromNumber $ sysStartUnixTime sysStart
  -- Add the system start time to the absolute time relative to system start
  -- to get overall POSIXTime
  pure $ wrap $ sysStartPosix + unwrap absTime
  where
  -- TODO: See https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley/impl/src/Cardano/Ledger/Shelley/HardForks.hs#L57
  -- translateTimeForPlutusScripts and ensure protocol version > 5 which would
  -- mean converting to milliseconds
  _transTime :: BigInt -> BigInt
  _transTime = (*) $ BigInt.fromInt 1000

-- | Finds the highest era end slot in `EraSummaries` (if any).
-- | `Nothing :: Maybe Slot` - is the highest slot that can ever be found.
highestEndSlotInEraSummaries
  :: EraSummaries
  -> Maybe (Maybe Slot)
highestEndSlotInEraSummaries =
  unwrap
    >>> map (unwrap >>> _.end >>> map (unwrap >>> _.slot))
    >>> maximum

-- | Finds the `EraSummary` an `Slot` lies inside (if any).
findSlotEraSummary
  :: EraSummaries
  -> Slot -- Slot we are testing and trying to find inside `EraSummaries`
  -> Either SlotToPosixTimeError EraSummary
findSlotEraSummary (EraSummaries eraSummaries) slot =
  note (CannotFindSlotInEraSummaries slot) $ find pred eraSummaries
  where
  slotNumber :: Slot -> BigInt
  slotNumber = unwrap >>> BigNum.toBigInt

  pred :: EraSummary -> Boolean
  pred (EraSummary { start, end }) =
    slotNumber (unwrap start).slot <= slotNumber slot
      && maybe true
        ((slotNumber slot < _) <<< slotNumber <<< _.slot <<< unwrap)
        end

-- This doesn't need to be exported but we can do it for tests.
-- | Relative slot of an `Slot` within an `EraSummary`
newtype RelSlot = RelSlot BigInt

derive instance Generic RelSlot _
derive instance Newtype RelSlot _
derive newtype instance Eq RelSlot
derive newtype instance Ord RelSlot
derive newtype instance DecodeAeson RelSlot
derive newtype instance EncodeAeson RelSlot

instance Show RelSlot where
  show (RelSlot rs) = showWithParens "RelSlot" rs

-- | Relative time to the start of an `EraSummary`. Contract this to
-- | `Ogmios.QueryM.RelativeTime` which is usually relative to system start.
-- | Treat as Milliseconds
newtype RelTime = RelTime BigInt

derive instance Generic RelTime _
derive instance Newtype RelTime _
derive newtype instance Eq RelTime
derive newtype instance Ord RelTime
derive newtype instance DecodeAeson RelTime
derive newtype instance EncodeAeson RelTime

instance Show RelTime where
  show (RelTime rt) = showWithParens "RelTime" rt

-- | Any leftover time from using `mod` when dividing my slot length.
-- | Treat as Milliseconds
newtype ModTime = ModTime BigInt

derive instance Generic ModTime _
derive instance Newtype ModTime _
derive newtype instance Eq ModTime
derive newtype instance Ord ModTime
derive newtype instance DecodeAeson ModTime
derive newtype instance EncodeAeson ModTime

instance Show ModTime where
  show (ModTime mt) = showWithParens "ModTime" mt

-- | Absolute time relative to System Start, not UNIX epoch.
-- | Treat as Milliseconds
newtype AbsTime = AbsTime BigInt

derive instance Generic AbsTime _
derive instance Newtype AbsTime _
derive newtype instance Eq AbsTime
derive newtype instance Ord AbsTime
derive newtype instance DecodeAeson AbsTime
derive newtype instance EncodeAeson AbsTime

instance Show AbsTime where
  show (AbsTime at) = showWithParens "AbsTime" at

-- | Find the relative slot provided we know the `Slot` for an absolute slot
-- | given an `EraSummary`. We could relax the `Either` monad if we use this
-- | in conjunction with `findSlotEraSummary`. However, we choose to make the
-- | function more general, guarding against a larger `start`ing slot
relSlotFromSlot
  :: EraSummary -> Slot -> Either SlotToPosixTimeError RelSlot
relSlotFromSlot (EraSummary { start }) s@(Slot slot) = do
  let
    startSlot = BigNum.toBigInt $ unwrap (unwrap start).slot
    biSlot = BigNum.toBigInt slot
  unless (startSlot <= biSlot) (throwError $ StartingSlotGreaterThanSlot s)
  pure $ wrap $ biSlot - startSlot

relTimeFromRelSlot :: EraSummary -> RelSlot -> Maybe RelTime
relTimeFromRelSlot eraSummary (RelSlot relSlot) =
  let
    slotLength = getSlotLength eraSummary
  in
    (<$>) wrap <<< BigInt.fromNumber $ (BigInt.toNumber relSlot) * slotLength

-- As justified in https://github.com/input-output-hk/ouroboros-network/blob/bd9e5653647c3489567e02789b0ec5b75c726db2/ouroboros-consensus/src/Ouroboros/Consensus/HardFork/History/Qry.hs#L461-L481
-- Treat the upperbound as inclusive.
-- | Returns the absolute time relative to some system start, not UNIX epoch.
absTimeFromRelTime
  :: EraSummary -> RelTime -> Either SlotToPosixTimeError AbsTime
absTimeFromRelTime (EraSummary { start, end }) (RelTime relTime) = do
  let
    startTime = unwrap (unwrap start).time * slotLengthFactor
    absTime = startTime + BigInt.toNumber relTime -- relative to System Start, not UNIX Epoch.
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    -- Note the hack that we don't have `end` for the current era, if we did not
    -- here could be issues going far into the future. But certain contracts are
    -- required to be in the distant future. Onchain, this uses POSIXTime which
    -- is stable, unlike Slots.
    endTime = maybe (absTime + one)
      ((*) slotLengthFactor <<< unwrap <<< _.time <<< unwrap)
      end
  unless
    (absTime <= endTime)
    (throwError $ EndTimeLessThanTime absTime)

  wrap <$> (liftM CannotGetBigIntFromNumber $ BigInt.fromNumber absTime)

--------------------------------------------------------------------------------
-- POSIXTime (milliseconds) to
-- Slot (absolute from System Start - see QueryM.SystemStart.getSystemStart)
--------------------------------------------------------------------------------
data PosixTimeToSlotError
  = CannotFindTimeInEraSummaries AbsTime
  | PosixTimeBeforeSystemStart POSIXTime
  | StartTimeGreaterThanTime AbsTime
  | EndSlotLessThanSlotOrModNonZero Slot ModTime
  | CannotGetBigIntFromNumber'
  | CannotGetBigNumFromBigInt'

derive instance Generic PosixTimeToSlotError _
derive instance Eq PosixTimeToSlotError

instance Show PosixTimeToSlotError where
  show = genericShow

posixTimeToSlotErrorStr :: String
posixTimeToSlotErrorStr = "posixTimeToSlotError"

instance EncodeAeson PosixTimeToSlotError where
  encodeAeson (CannotFindTimeInEraSummaries absTime) =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "cannotFindTimeInEraSummaries"
      [ absTime ]
  encodeAeson (PosixTimeBeforeSystemStart posixTime) =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "posixTimeBeforeSystemStart"
      [ posixTime ]
  encodeAeson (StartTimeGreaterThanTime absTime) =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "startTimeGreaterThanTime"
      [ absTime ]
  encodeAeson (EndSlotLessThanSlotOrModNonZero slot modTime) =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "endSlotLessThanSlotOrModNonZero"
      [ encodeAeson slot, encodeAeson modTime ]
  encodeAeson CannotGetBigIntFromNumber' =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "cannotGetBigIntFromNumber'"
      aesonNull
  encodeAeson CannotGetBigNumFromBigInt' =
    encodeAeson $ mkErrorRecord
      posixTimeToSlotErrorStr
      "cannotGetBigNumFromBigInt'"
      aesonNull

instance DecodeAeson PosixTimeToSlotError where
  decodeAeson = aesonObject $ \o -> do
    errorType <- getField o "errorType"
    unless (errorType == posixTimeToSlotErrorStr)
      $ throwError
      $ TypeMismatch "Expected PosixTimeToSlotError"
    getField o "error" >>= case _ of
      "cannotFindTimeInEraSummaries" -> do
        arg <- extractArg o
        pure $ CannotFindTimeInEraSummaries arg
      "posixTimeBeforeSystemStart" -> do
        arg <- extractArg o
        pure $ PosixTimeBeforeSystemStart arg
      "startTimeGreaterThanTime" -> do
        arg <- extractArg o
        pure $ StartTimeGreaterThanTime arg
      "endSlotLessThanSlotOrModNonZero" -> do
        args <- getField o "args"
        when (length args /= 2)
          (throwError $ TypeMismatch "Incorrect args")
        as <- decodeAeson =<< note
          (TypeMismatch "Could not extract first element")
          (index args 0)
        mt <- decodeAeson =<< note
          (TypeMismatch "Could not extract second element")
          (index args 1)
        pure $ EndSlotLessThanSlotOrModNonZero as mt
      "cannotGetBigIntFromNumber'" -> do
        args <- getField o "args"
        unless (isNull args) (throwError $ TypeMismatch "Non-empty args")
        pure CannotGetBigIntFromNumber'
      "cannotGetBigNumFromBigInt'" -> do
        args <- getField o "args"
        unless (isNull args) (throwError $ TypeMismatch "Non-empty args")
        pure CannotGetBigNumFromBigInt'
      _ -> throwError $ TypeMismatch "Unknown error message"

-- | Converts a `POSIXTime` to `Slot` given an `EraSummaries` and
-- | `SystemStart` queried from Ogmios.
posixTimeToSlot
  :: EraSummaries
  -> SystemStart
  -> POSIXTime
  -> Effect (Either PosixTimeToSlotError Slot)
posixTimeToSlot eraSummaries sysStart pt'@(POSIXTime pt) = runExceptT do
  -- Get POSIX time for system start:
  sysStartPosix <- liftM CannotGetBigIntFromNumber' $ sysStartUnixTime sysStart
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
  relSlotMod <- liftM CannotGetBigIntFromNumber' $ relSlotFromRelTime currentEra
    relTime
  -- Get absolute slot relative to system start
  liftEither $ slotFromRelSlot currentEra relSlotMod

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
    let
      numberAt = BigInt.toNumber at
    in
      unwrap (unwrap start).time * slotLengthFactor <= numberAt
        && maybe true
          ( (<) numberAt <<< (*) slotLengthFactor <<< unwrap <<< _.time <<<
              unwrap
          )
          end

relTimeFromAbsTime
  :: EraSummary -> AbsTime -> Either PosixTimeToSlotError RelTime
relTimeFromAbsTime (EraSummary { start }) at@(AbsTime absTime) = do
  let startTime = unwrap (unwrap start).time * slotLengthFactor
  unless (startTime <= BigInt.toNumber absTime)
    (throwError $ StartTimeGreaterThanTime at)
  let
    relTime = BigInt.toNumber absTime - startTime -- relative to era start, not UNIX Epoch.
  wrap <$>
    ( note CannotGetBigIntFromNumber'
        <<< BigInt.fromNumber
        <<< Math.trunc
    ) relTime

-- | Converts relative time to relative slot (using Euclidean division) and
-- | modulus for any leftover.
relSlotFromRelTime
  :: EraSummary -> RelTime -> Maybe (RelSlot /\ ModTime)
relSlotFromRelTime eraSummary (RelTime relTime) =
  let
    slotLength = getSlotLength eraSummary
    relSlot = wrap <$>
      (BigInt.fromNumber <<< Math.trunc) (BigInt.toNumber relTime / slotLength)
    modTime = wrap <$>
      BigInt.fromNumber (BigInt.toNumber relTime Math.% slotLength)
  in
    (/\) <$> relSlot <*> modTime

slotFromRelSlot
  :: EraSummary -> RelSlot /\ ModTime -> Either PosixTimeToSlotError Slot
slotFromRelSlot
  (EraSummary { start, end })
  (RelSlot relSlot /\ mt@(ModTime modTime)) = do
  let
    startSlot = BigNum.toBigInt $ unwrap (unwrap start).slot
    -- Round down to the nearest Slot to accept Milliseconds as input.
    slot = startSlot + relSlot -- relative to system start
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    -- Note the hack that we don't have `end` for the current era, if we did not
    -- here could be issues going far into the future. But certain contracts are
    -- required to be in the distant future. Onchain, this uses POSIXTime which
    -- is stable, unlike Slots.
    endSlot = maybe (slot + one)
      (BigNum.toBigInt <<< unwrap <<< _.slot <<< unwrap)
      end
  bnSlot <- liftM CannotGetBigNumFromBigInt' $ BigNum.fromBigInt slot
  -- Check we are less than the end slot, or if equal, there is no excess:
  unless (slot < endSlot || slot == endSlot && modTime == zero)
    (throwError $ EndSlotLessThanSlotOrModNonZero (wrap bnSlot) mt)
  pure $ wrap bnSlot

-- | Get SlotLength in Milliseconds
getSlotLength :: EraSummary -> Number
getSlotLength (EraSummary { parameters }) =
  unwrap (unwrap parameters).slotLength

--------------------------------------------------------------------------------

sequenceInterval
  :: forall (a :: Type) (m :: Type -> Type)
   . Applicative m
  => Interval (m a)
  -> m (Interval a)
sequenceInterval (FiniteInterval start end) = FiniteInterval <$> start <*> end
sequenceInterval (StartAt end) = StartAt <$> end
sequenceInterval (EndAt start) = EndAt <$> start
sequenceInterval EmptyInterval = pure EmptyInterval
sequenceInterval AlwaysInterval = pure AlwaysInterval

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
  range = sequenceInterval <$>
  sequenceInterval (posixTimeToSlot eraSummaries sysStart <$> range)

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
  range = sequenceInterval <$>
  sequenceInterval (slotToPosixTime eraSummaries sysStart <$> range)

type TransactionValiditySlot =
  { validityStartInterval :: Maybe Slot, timeToLive :: Maybe Slot }

-- | Converts a `SlotRange` to two separate slots used in building
-- | Cardano.Types.Transaction.
-- | `Nothing` for `validityStartInterval` represents `Slot zero`.
-- | `Nothing` for `timeToLive` represents `maxSlot`.
-- | The only finite interval we can represent is of the form [a,b)
-- https://github.com/input-output-hk/cardano-ledger/blob/94b1ae3d6b66f4232f34060d89c2f12628998ef2/eras/shelley-ma/impl/src/Cardano/Ledger/ShelleyMA/Timelocks.hs#L100-L106
slotRangeToTransactionValidity
  :: SlotRange
  -> TransactionValiditySlot
slotRangeToTransactionValidity (FiniteInterval start end) =
  { validityStartInterval: pure start
  , timeToLive: wrap <$> BigNum.add BigNum.one (unwrap end)
  }
slotRangeToTransactionValidity (StartAt end) =
  { validityStartInterval: Nothing
  , timeToLive: wrap <$> BigNum.add BigNum.one (unwrap end)
  }
slotRangeToTransactionValidity (EndAt start) =
  { validityStartInterval: pure start, timeToLive: Nothing }
slotRangeToTransactionValidity AlwaysInterval =
  { validityStartInterval: Nothing, timeToLive: Nothing }
slotRangeToTransactionValidity EmptyInterval =
  { validityStartInterval: pure maxSlot, timeToLive: pure $ wrap BigNum.zero }

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
  encodeAeson = encodeAeson <<< unwrap

instance (DecodeAeson a) => DecodeAeson (HaskInterval a) where
  decodeAeson a = lmap (Named "Interval") $ HaskInterval <$> decodeAeson a

instance (EncodeAeson a) => EncodeAeson (LowerBound a) where
  encodeAeson (LowerBound a b) = encodeAeson (a /\ b)

instance (DecodeAeson a) => DecodeAeson (LowerBound a) where
  decodeAeson a = uncurry LowerBound <$> decodeAeson a

instance (EncodeAeson a) => EncodeAeson (UpperBound a) where
  encodeAeson (UpperBound a b) = encodeAeson (a /\ b)

instance (DecodeAeson a) => DecodeAeson (UpperBound a) where
  decodeAeson a = uncurry UpperBound <$> decodeAeson a

instance (EncodeAeson a) => EncodeAeson (Extended a) where
  encodeAeson = case _ of
    NegInf -> encodeAeson { tag: "NegInf" }
    Finite a -> encodeTagged' "Finite" a
    PosInf -> encodeAeson { tag: "PosInf" }

instance (DecodeAeson a) => DecodeAeson (Extended a) where
  decodeAeson a = lmap (Named "Extended") do
    obj <- decodeAeson a
    tag <- obj .: tagProp
    case tag of
      "NegInf" -> pure NegInf
      "PosInf" -> pure PosInf
      "Finite" -> Finite <$> obj .: contentsProp
      _ -> Left $ AtKey tagProp $ UnexpectedValue $ encodeString tag

toOnChainPosixTimeRangeErrorStr :: String
toOnChainPosixTimeRangeErrorStr = "ToOnChainPosixTimeRangeError"

instance EncodeAeson ToOnChainPosixTimeRangeError where
  encodeAeson (PosixTimeToSlotError' err) =
    encodeAeson $ mkErrorRecord
      toOnChainPosixTimeRangeErrorStr
      "posixTimeToSlotError'"
      [ err ]
  encodeAeson (SlotToPosixTimeError' err) =
    encodeAeson $ mkErrorRecord
      toOnChainPosixTimeRangeErrorStr
      "slotToPosixTimeError'"
      [ err ]

instance DecodeAeson ToOnChainPosixTimeRangeError where
  decodeAeson = aesonObject $ \o -> do
    errorType <- getField o "errorType"
    unless (errorType == toOnChainPosixTimeRangeErrorStr)
      $ throwError
      $ TypeMismatch "Expected ToOnChainPosixTimeRangeError"
    getField o "error" >>= case _ of
      "posixTimeToSlotError'" -> do
        arg <- extractArg o
        pure $ PosixTimeToSlotError' arg
      "slotToPosixTimeError'" -> do
        arg <- extractArg o
        pure $ SlotToPosixTimeError' arg
      _ -> throwError $ TypeMismatch "Unknown error message"

intervalToHaskInterval
  :: forall (a :: Type). Ord a => Semiring a => Interval a -> HaskInterval a
intervalToHaskInterval (FiniteInterval start end) = wrap
  { ivFrom: lowerBound start
  , ivTo: strictUpperBound (end + one)
  }
intervalToHaskInterval (StartAt end) = wrap
  { ivFrom: LowerBound NegInf true
  , ivTo: strictUpperBound (end + one)
  }
intervalToHaskInterval (EndAt start) = wrap
  { ivFrom: lowerBound start
  , ivTo: UpperBound PosInf true
  }
intervalToHaskInterval AlwaysInterval = wrap
  { ivFrom: LowerBound NegInf true
  , ivTo: UpperBound PosInf true
  }
intervalToHaskInterval EmptyInterval = wrap
  { ivFrom: LowerBound PosInf true
  , ivTo: UpperBound NegInf true
  }

haskIntervalToInterval
  :: forall (a :: Type)
   . Ring a
  => Ord a
  => HaskInterval a
  -> Interval a
haskIntervalToInterval (HaskInterval { ivFrom, ivTo }) = case ivFrom, ivTo of
  LowerBound (Finite start) true, UpperBound (Finite end) false ->
    mkFiniteInterval start (end - one)
  LowerBound NegInf _, UpperBound (Finite end) false -> StartAt $ (end - one)
  LowerBound (Finite start) true, UpperBound PosInf _ -> EndAt start
  LowerBound NegInf _, UpperBound PosInf _ -> AlwaysInterval
  LowerBound PosInf _, UpperBound NegInf _ -> EmptyInterval
  -- All of the remain cases are to avoid using a `Maybe` or throwing exception.
  LowerBound (Finite start) true, UpperBound (Finite end) true ->
    mkFiniteInterval start end
  LowerBound (Finite start) false, UpperBound (Finite end) true ->
    mkFiniteInterval (start + one) end
  LowerBound (Finite start) false, UpperBound (Finite end) false ->
    mkFiniteInterval (start + one) (end - one)
  LowerBound NegInf _, UpperBound (Finite end) true -> StartAt end
  LowerBound (Finite start) false, UpperBound PosInf _ -> EndAt $ (start + one)
  LowerBound _ _, UpperBound NegInf _ -> EmptyInterval
  LowerBound PosInf _, UpperBound _ _ -> EmptyInterval

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
      liftEither $ Right $ wrap $ mkFiniteInterval t1 t2
