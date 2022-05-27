module Time.Conversion
  ( AbsTime(..)
  , ModTime(..)
  , PosixTimeToSlotError(..)
  , RelTime(..)
  , SlotToPosixTimeError(..)
  , ToOnChainPosixTimeRangeError(..)
  , absSlotFromSlot
  , findSlotEraSummary
  , findTimeEraSummary
  , posixTimeRangeToSlotRange
  , posixTimeRangeToTransactionValidity
  , posixTimeToSlot
  , slotFromAbsSlot
  , slotRangeToPosixTimeRange
  , slotRangeToTransactionValidity
  , slotToPosixTime
  , toOnchainPosixTimeRange
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Data.Array (find)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber) as BigInt
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.JSDate (getTime, parse)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Helpers (bigIntToUInt, liftEither, liftM, uIntToBigInt)
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , EraSummariesQR(EraSummariesQR)
  , EraSummary(EraSummary)
  , SystemStartQR
  )
import Serialization.Address (Slot(Slot))
import Time.Types.Interval
  ( Extended(Finite, PosInf, NegInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , UpperBound(UpperBound)
  , always
  , from
  , interval
  , to
  )
import Time.Types.POSIXTime
  ( OnchainPOSIXTimeRange
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  )
import Time.Types.Slot (SlotRange, maxSlot)

--------------------------------------------------------------------------------
-- Slot (absolute from System Start - see QueryM.SystemStart.getSystemStart)
-- to POSIXTime (milliseconds)
--------------------------------------------------------------------------------
data SlotToPosixTimeError
  = ParseToDataTime SystemStartQR
  | CannotFindSlotInEraSummaries AbsSlot
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
  :: EraSummariesQR
  -> SystemStartQR
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
  :: EraSummariesQR
  -> AbsSlot -- Slot we are testing and trying to find inside `EraSummariesQR`
  -> Either SlotToPosixTimeError EraSummary
findSlotEraSummary (EraSummariesQR eraSummaries) os =
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
  | EndSlotLessThanSlot AbsSlot
  | RelModNonZero ModTime
  | CannotConvertAbsSlotToSlot AbsSlot
  | CannotGetBigIntFromNumber'

derive instance Generic PosixTimeToSlotError _
derive instance Eq PosixTimeToSlotError

instance Show PosixTimeToSlotError where
  show = genericShow

-- | Converts a `POSIXTime` to `Slot` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
posixTimeToSlot
  :: EraSummariesQR
  -> SystemStartQR
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
  :: EraSummariesQR
  -> AbsTime -- Time we are testing and trying to find inside `EraSummariesQR`
  -> Either PosixTimeToSlotError EraSummary
findTimeEraSummary (EraSummariesQR eraSummaries) absTime@(AbsTime at) =
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
  es@(EraSummary { start, end })
  (RelSlot relSlot /\ (ModTime modTime)) = do
  let
    startSlot = unwrap (unwrap start).slot
    slotLength = getSlotLength es
    -- Round to the nearest Slot to accept Milliseconds as input.
    -- Potential FIXME: perhaps we always need to round down.
    roundedModSlot =
      if modTime >= slotLength / BigInt.fromInt 2 then one else zero
    absSlot = startSlot + relSlot + roundedModSlot -- relative to system start
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    endSlot = maybe (absSlot + one) (unwrap <<< _.slot <<< unwrap) end
  unless (absSlot <= endSlot) (throwError $ EndSlotLessThanSlot $ wrap absSlot)
  -- Potential FIXME: drop this constraint and round down to the nearest slot
  -- see roundedModTime.
  -- Check for no remainder:
  -- unless (modTime == zero) (throwError $ RelModNonZero mt)
  -- Potential FIXME: Do we want to use `safeZone` from `parameters`.
  pure $ wrap absSlot

-- | Get SlotLength in Milliseconds
getSlotLength :: EraSummary -> BigInt
getSlotLength (EraSummary { parameters }) =
  unwrap (unwrap parameters).slotLength * factor

--------------------------------------------------------------------------------

-- | Converts a `POSIXTimeRange` to `SlotRange` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
posixTimeRangeToSlotRange
  :: EraSummariesQR
  -> SystemStartQR
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

-- | Converts a `SlotRange` to `POSIXTimeRange` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
slotRangeToPosixTimeRange
  :: EraSummariesQR
  -> SystemStartQR
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
  validityStartInterval :: Maybe Slot
  validityStartInterval = case start, startInc of
    Finite s, true -> pure s
    Finite s, false -> pure $ s <> Slot one -- This could be suspect
    NegInf, _ -> Nothing
    PosInf, _ -> pure maxSlot

  timeToLive :: Maybe Slot
  timeToLive = case end, endInc of
    Finite s, true -> pure s
    Finite s, false -> pure $ s <> Slot (negate one) -- This could be suspect
    NegInf, _ -> pure $ Slot zero
    PosInf, _ -> Nothing

-- | Converts a `POSIXTimeRange` to a transaction validity interval via a
-- | `SlotRange` to be used when building a CSL transaction body
posixTimeRangeToTransactionValidity
  :: EraSummariesQR
  -> SystemStartQR
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

-- https://github.com/input-output-hk/cardano-ledger/blob/2acff66e84d63a81de904e1c0de70208ff1819ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L206-L226
-- | Create an `OnchainPOSIXTimeRange` to do a round trip from an off-chain
-- | POSIXTimeRange as follows:
-- | 1) `POSIXTimeRange` -> `SlotRange`
-- | 2) `SlotRange` -> `TransactionValidity`
-- | 3) `TransactionValidity` -> `OnchainPOSIXTimeRange`
-- | `OnchainPOSIXTimeRange` is intended to equal the validity range found in
-- | the on-chain `ScriptContext`
toOnchainPosixTimeRange
  :: EraSummariesQR
  -> SystemStartQR
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
