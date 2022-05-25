module Time.Conversion
  ( AbsTime(..)
  , SlotToPosixTimeError(..)
  , slotToPosixTime
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber) as BigInt
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find)
import Data.Generic.Rep (class Generic)
import Data.JSDate (getTime, parse)
import Data.Either (Either, note)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Helpers (liftEither, liftM, uIntToBigInt)
import QueryM (QueryM)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , EraSummariesQR(EraSummariesQR)
  , EraSummary(EraSummary)
  , SystemStartQR
  )
import QueryM.SystemStart (getSystemStart)
import Serialization.Address (Slot)
import Types.Interval (POSIXTime)

--------------------------------------------------------------------------------
-- Slot (absolute from System Start- see QueryM.SystemStart.getSystemStart)
-- to POSIXTime (milliseconds)
--------------------------------------------------------------------------------
data SlotToPosixTimeError
  = ParseToDataTime SystemStartQR
  | CannotFindSlotInEraSummaries AbsSlot
  | StartingSlotGreaterThanSlot AbsSlot
  | EndTimeLessThanTime AbsTime
  | CannotGetBigIntFromNumber

-- | SlotFromAbsSlot AbsSlot -- perhaps remove

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
-- | Converts a CSL (Absolute) `Slot` (Unsigned Integer) to `POSIXTime` which
-- | is time elapsed from January 1, 1970 (midnight UTC/GMT). We obtain this
-- | By converting `Slot` to `AbsTime` which is time relative to some System
-- | Start, then add any excess for a UNIX Epoch time. Recall that POSIXTime
-- | is in milliseconds for Protocol Version >= 6.
slotToPosixTime :: Slot -> QueryM (Either SlotToPosixTimeError POSIXTime)
slotToPosixTime slot = runExceptT do
  let ogmiosSlot = absSlotFromSlot slot
  -- Get system start and parse into `DateTime`:
  sysStart <- lift getSystemStart
  -- Get JSDate:
  sysStartD <- liftEffect $ parse $ unwrap sysStart
  -- Get era summaries:
  eraSummaries <- lift getEraSummaries
  -- Find current era:
  currentEra <- liftEither $ findEraSummary eraSummaries ogmiosSlot
  -- Convert absolute slot (relative to System start) to relative slot of era
  relSlot <- liftEither $ relSlotFromAbsSlot currentEra ogmiosSlot
  -- Convert relative slot to relative time for that era
  let relTime = relTimeFromRelSlot currentEra relSlot
  absTime <- liftEither $ absTimeFromRelTime currentEra relTime
  -- Get POSIX time for system start
  sysStartPosix <- liftM CannotGetBigIntFromNumber
    $ BigInt.fromNumber
    $ getTime sysStartD
  -- Add the system start time to the absolute time relative to system start
  -- to get overall POSIXTime, then convert to milliseconds
  pure $ wrap $ transTime $ sysStartPosix + unwrap absTime
  where
  -- TODO: See https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley/impl/src/Cardano/Ledger/Shelley/HardForks.hs#L57
  -- translateTimeForPlutusScripts and ensure protocol version > 5 which would
  -- mean converting to milliseconds
  transTime :: BigInt -> BigInt
  transTime = (*) $ BigInt.fromInt 1000 -- to milliseconds

-- | Convert a CSL (Absolute) `Slot` (`UInt`) to an Ogmios absolute slot
-- | (`BigInt`)
absSlotFromSlot :: Slot -> AbsSlot
absSlotFromSlot = wrap <<< uIntToBigInt <<< unwrap

-- -- | Convert an Ogmios absolute slot (`BigInt`) to a CSL (Absolute) `Slot`
-- -- | (`UInt`)
-- slotFromAbsSlot :: AbsSlot -> Maybe Slot
-- slotFromAbsSlot = map wrap <<< bigIntToUInt <<< unwrap

-- | Finds the `EraSummary` an `AbsSlot` lies inside (if any).
findEraSummary
  :: EraSummariesQR
  -> AbsSlot -- Slot we are testing and trying to find inside `EraSummariesQR`
  -> Either SlotToPosixTimeError EraSummary
findEraSummary (EraSummariesQR eraSummaries) os =
  note (CannotFindSlotInEraSummaries os) $ find pred eraSummaries
  where
  -- Potential FIXME: In the case of `Just`, do we want to use `safeZone` from
  -- `parameters` to provide a buffer?
  pred :: EraSummary -> Boolean
  pred (EraSummary { start, end }) =
    (unwrap start).slot <= os && maybe true ((<) os <<< _.slot <<< unwrap) end

-- getEstAbsSlot :: EraSummaryTime -> AbsSlot
-- getEstAbsSlot (EraSummaryTime es) = es.slot

-- | Relative slot of an `AbsSlot` within an `EraSummary`
newtype RelSlot = RelSlot BigInt

derive instance Generic RelSlot _
derive instance Newtype RelSlot _
derive newtype instance Eq RelSlot
derive newtype instance Ord RelSlot

instance Show RelSlot where
  show = genericShow

-- | Relative time to the start of an `EraSummary`
newtype RelTime = RelTime BigInt

derive instance Generic RelTime _
derive instance Newtype RelTime _
derive newtype instance Eq RelTime
derive newtype instance Ord RelTime

instance Show RelTime where
  show = genericShow

-- | Absolute time relative to System Start, not UNIX epoch.
newtype AbsTime = AbsTime BigInt

derive instance Generic AbsTime _
derive instance Newtype AbsTime _
derive newtype instance Eq AbsTime
derive newtype instance Ord AbsTime

instance Show AbsTime where
  show = genericShow

-- | Find the relative slot provided we know the `AbsSlot` for an absolute slot
-- | given an `EraSummary`. We could relax the `Maybe` monad if we use this
-- | in conjunction with `findEraSummary`. However, we choose to make the
-- | function more general, guarding against a larger `start`ing slot
relSlotFromAbsSlot
  :: EraSummary -> AbsSlot -> Either SlotToPosixTimeError RelSlot
relSlotFromAbsSlot (EraSummary { start }) os@(AbsSlot ogmiosSlot) = do
  let startSlot = unwrap (unwrap start).slot
  unless (startSlot <= ogmiosSlot) (throwError $ StartingSlotGreaterThanSlot os)
  pure $ wrap $ ogmiosSlot - startSlot

relTimeFromRelSlot :: EraSummary -> RelSlot -> RelTime
relTimeFromRelSlot (EraSummary { parameters }) (RelSlot relSlot) =
  let
    slotLength = unwrap (unwrap parameters).slotLength
  in
    wrap $ relSlot * slotLength

-- As justified in https://github.com/input-output-hk/ouroboros-network/blob/bd9e5653647c3489567e02789b0ec5b75c726db2/ouroboros-consensus/src/Ouroboros/Consensus/HardFork/History/Qry.hs#L461-L481
-- Treat the upperbound as inclusive.
-- | Returns the absolute time relative to some system start, not UNIX epoch.
absTimeFromRelTime
  :: EraSummary -> RelTime -> Either SlotToPosixTimeError AbsTime
absTimeFromRelTime (EraSummary { start, end }) (RelTime relTime) = do
  let
    startTime = unwrap (unwrap start).time
    absTime = startTime + relTime -- relative to System Start, not UNIX Epoch.
    -- If `EraSummary` doesn't have an end, the condition is automatically
    -- satisfied. We use `<=` as justified by the source code.
    endTime = maybe (absTime + one) (unwrap <<< _.slot <<< unwrap) end
  unless (absTime <= endTime) (throwError $ EndTimeLessThanTime $ wrap absTime)
  pure $ wrap absTime

