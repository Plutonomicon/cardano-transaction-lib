-- | A module containing time-related datatypes and helpers.
module CTL.Contract.Time
  ( getCurrentEpoch
  , getEraSummaries
  , getSystemStart
  , module Chain
  , module ExportOgmios
  , module Interval
  , module SerializationAddress
  ) where

import CTL.Contract.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  , getTip
  ) as Chain
import CTL.Contract.Monad (Contract, wrapContract)
import CTL.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as CurrentEpoch
import CTL.Internal.QueryM.EraSummaries (getEraSummaries) as EraSummaries
import CTL.Internal.QueryM.Ogmios
  ( CurrentEpoch
  , EraSummaries
  , SystemStart
  )
import CTL.Internal.QueryM.Ogmios
  ( CurrentEpoch(CurrentEpoch)
  , Epoch(Epoch)
  , EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , RelativeTime(RelativeTime)
  , SafeZone(SafeZone)
  , SlotLength(SlotLength)
  , SystemStart(SystemStart)
  ) as ExportOgmios
import CTL.Internal.QueryM.SystemStart (getSystemStart) as SystemStart
import CTL.Internal.Serialization.Address
  ( BlockId(BlockId)
  , Slot(Slot)
  ) as SerializationAddress
import CTL.Internal.Types.Interval
  ( AbsTime(AbsTime)
  , Closure
  , Extended(NegInf, Finite, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , ModTime(ModTime)
  , OnchainPOSIXTimeRange(OnchainPOSIXTimeRange)
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  , PosixTimeToSlotError
      ( CannotFindTimeInEraSummaries
      , PosixTimeBeforeSystemStart
      , StartTimeGreaterThanTime
      , EndSlotLessThanSlotOrModNonZero
      , CannotGetBigIntFromNumber'
      )
  , RelTime(RelTime)
  , SlotRange
  , SlotToPosixTimeError
      ( CannotFindSlotInEraSummaries
      , StartingSlotGreaterThanSlot
      , EndTimeLessThanTime
      , CannotGetBigIntFromNumber
      )
  , ToOnChainPosixTimeRangeError(PosixTimeToSlotError', SlotToPosixTimeError')
  , UpperBound(UpperBound)
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
  -- , isEmpty'
  , lowerBound
  , maxSlot
  , member
  , mkInterval
  , never
  , overlaps
  -- , overlaps'
  , posixTimeRangeToSlotRange
  , posixTimeToSlot
  , singleton
  , slotRangeToPosixTimeRange
  , slotToPosixTime
  , strictLowerBound
  , strictUpperBound
  , to
  , toOnchainPosixTimeRange
  , upperBound
  ) as Interval

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: forall (r :: Row Type). Contract r CurrentEpoch
getCurrentEpoch = wrapContract CurrentEpoch.getCurrentEpoch

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: forall (r :: Row Type). Contract r EraSummaries
getEraSummaries = wrapContract EraSummaries.getEraSummaries

-- | Get the current system start time. Details can be found
-- | https://ogmios.dev/api/ under "systemStart" query
getSystemStart :: forall (r :: Row Type). Contract r SystemStart
getSystemStart = wrapContract SystemStart.getSystemStart
