-- | A module containing time-related datatypes and helpers.
module Contract.Time
  ( getCurrentEpoch
  , getEraSummaries
  , getSystemStart
  , module Chain
  , module ExportConversion
  , module ExportOgmios
  , module ExportSlot
  , module Interval
  , module POSIXTime
  , module SerializationAddress
  ) where

import Contract.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  , getTip
  ) as Chain
import Contract.Monad (Contract, wrapContract)
import QueryM.CurrentEpoch (getCurrentEpoch) as CurrentEpoch
import QueryM.EraSummaries (getEraSummaries) as EraSummaries
import QueryM.Ogmios
  ( CurrentEpochQR
  , EraSummariesQR
  , SystemStartQR
  )
import QueryM.Ogmios
  ( AbsSlot(AbsSlot)
  , CurrentEpochQR(CurrentEpochQR)
  , Epoch(Epoch)
  , EpochLength(EpochLength)
  , EraSummariesQR(EraSummariesQR)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , RelativeTime(RelativeTime)
  , SafeZone(SafeZone)
  , SlotLength(SlotLength)
  , SystemStartQR(SystemStartQR)
  ) as ExportOgmios
import QueryM.SystemStart (getSystemStart) as SystemStart
import Serialization.Address
  ( Slot(Slot)
  , BlockId(BlockId)
  ) as SerializationAddress
import Time.Conversion
  ( AbsTime(AbsTime)
  , ModTime(ModTime)
  , PosixTimeToSlotError
      ( CannotFindTimeInEraSummaries
      , PosixTimeBeforeSystemStart
      , StartTimeGreaterThanTime
      , EndSlotLessThanSlotOrModNonZero
      , CannotConvertAbsSlotToSlot
      , CannotGetBigIntFromNumber'
      )
  , RelTime(RelTime)
  , SlotToPosixTimeError
      ( ParseToDataTime
      , CannotFindSlotInEraSummaries
      , StartingSlotGreaterThanSlot
      , EndTimeLessThanTime
      , CannotGetBigIntFromNumber
      )
  , ToOnChainPosixTimeRangeError(PosixTimeToSlotError', SlotToPosixTimeError')
  , absSlotFromSlot
  , findSlotEraSummary
  , findTimeEraSummary
  , posixTimeRangeToSlotRange
  , posixTimeToSlot
  , slotFromAbsSlot
  , slotRangeToPosixTimeRange
  , slotToPosixTime
  , toOnchainPosixTimeRange
  ) as ExportConversion
import Time.Types.Interval
  ( Closure
  , Extended(NegInf, Finite, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , UpperBound(UpperBound)
  , after
  , always
  , before
  , contains
  , from
  , hull
  , intersection
  , interval
  , isEmpty
  -- , isEmpty'
  , lowerBound
  , member
  , mkInterval
  , never
  , overlaps
  -- , overlaps'
  , singleton
  , strictLowerBound
  , strictUpperBound
  , to
  , upperBound
  ) as Interval
import Time.Types.POSIXTime
  ( OnchainPOSIXTimeRange(OnchainPOSIXTimeRange)
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  ) as POSIXTime
import Time.Types.Slot
  ( SlotRange
  , beginningOfTime
  , maxSlot
  ) as ExportSlot

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: forall (r :: Row Type). Contract r CurrentEpochQR
getCurrentEpoch = wrapContract CurrentEpoch.getCurrentEpoch

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: forall (r :: Row Type). Contract r EraSummariesQR
getEraSummaries = wrapContract EraSummaries.getEraSummaries

-- | Get the current system start time. Details can be found
-- | https://ogmios.dev/api/ under "systemStart" query
getSystemStart :: forall (r :: Row Type). Contract r SystemStartQR
getSystemStart = wrapContract SystemStart.getSystemStart

