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
import Data.BigInt (BigInt, quot)
import Data.BigInt (fromString, fromInt) as BigInt
import Data.Enum (class Enum, succ)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  )
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromString) as UInt
import FromData (class FromData, genericFromData)
import Helpers (uIntToBigInt, bigIntToUInt)
import Partial.Unsafe (unsafePartial)
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
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
  toData e = genericToData e

instance FromData a => FromData (Extended a) where
  fromData e = genericFromData e

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
  toData lb = genericToData lb

instance FromData a => FromData (LowerBound a) where
  fromData lb = genericFromData lb

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
  toData ub = genericToData ub

instance FromData a => FromData (UpperBound a) where
  fromData ub = genericFromData ub

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
  toData i = genericToData i

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
  (Interval { from: LowerBound start startIncl, to: UpperBound end endIncl }) =
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
      { from: LowerBound lbound startIncl
      , to: UpperBound ubound endIncl
      }

-- | Convert a `Slot` to a `POSIXTimeRange` given a `SlotConfig`. Each `Slot`
-- | can be represented by an interval of time.
slotToPOSIXTimeRange :: SlotConfig -> Slot -> POSIXTimeRange
slotToPOSIXTimeRange sc slot =
  interval (slotToBeginPOSIXTime sc slot) (slotToEndPOSIXTime sc slot)

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
      { from: LowerBound start startIncl
      , to: UpperBound end endIncl
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
    { from: LowerBound start'
        (closureWith slotToBeginPOSIXTime startIncl start')
    , to: UpperBound end' (closureWith slotToBeginPOSIXTime endIncl end')
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
  (Interval { from: LowerBound start startIncl, to: UpperBound end endIncl }) =
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
  encodeAeson' x = pure $
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
  encodeAeson' x = pure $
    ( defer \_ -> E.encode $ (case _ of LowerBound a b -> (a /\ b)) >$<
        (E.tuple (E.value >/\< E.value))
    ) x

instance (DecodeAeson a) => DecodeAeson (LowerBound a) where
  decodeAeson = defer \_ -> D.decode $
    (D.tuple $ LowerBound </$\> D.value </*\> D.value)

instance (EncodeAeson a) => EncodeAeson (UpperBound a) where
  encodeAeson' x = pure $
    ( defer \_ -> E.encode $ (case _ of UpperBound a b -> (a /\ b)) >$<
        (E.tuple (E.value >/\< E.value))
    ) x

instance (DecodeAeson a) => DecodeAeson (UpperBound a) where
  decodeAeson = defer \_ -> D.decode $
    (D.tuple $ UpperBound </$\> D.value </*\> D.value)

instance (EncodeAeson a) => EncodeAeson (Extended a) where
  encodeAeson' x = pure $
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
