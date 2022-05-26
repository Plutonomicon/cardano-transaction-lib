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
  , posixTimeRangeToSlotRange
  , posixTimeToSlot
  , slotRangeToPosixTimeRange
  , slotToPosixTime
  , toOnchainPosixTimeRange
  ) where

import Contract.Prelude

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
      , EndSlotLessThanSlot
      , RelModNonZero
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
  , slotFromAbsSlot
  ) as ExportConversion
import Time.Conversion
  ( posixTimeRangeToSlotRange
  , posixTimeToSlot
  , slotRangeToPosixTimeRange
  , slotToPosixTime
  , toOnchainPosixTimeRange
  ) as Conversion
import Time.Conversion
  ( PosixTimeToSlotError
  , SlotToPosixTimeError
  , ToOnChainPosixTimeRangeError
  )
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
import Time.Types.Slot (Slot, SlotRange)

-- | Converts a `POSIXTimeRange` to `SlotRange` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
posixTimeRangeToSlotRange
  :: forall (r :: Row Type)
   . EraSummariesQR
  -> SystemStartQR
  -> POSIXTime.POSIXTimeRange
  -> Contract r (Either PosixTimeToSlotError SlotRange)
posixTimeRangeToSlotRange es ss =
  wrapContract <<< Conversion.posixTimeRangeToSlotRange es ss

-- | Converts a `POSIXTime` to `Slot` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
posixTimeToSlot
  :: forall (r :: Row Type)
   . EraSummariesQR
  -> SystemStartQR
  -> POSIXTime.POSIXTime
  -> Contract r (Either PosixTimeToSlotError Slot)
posixTimeToSlot es ss =
  wrapContract <<< Conversion.posixTimeToSlot es ss

-- | Converts a `SlotRange` to `POSIXTimeRange` given an `EraSummariesQR` and
-- | `SystemStartQR` queried from Ogmios.
slotRangeToPosixTimeRange
  :: forall (r :: Row Type)
   . EraSummariesQR
  -> SystemStartQR
  -> SlotRange
  -> Contract r (Either SlotToPosixTimeError POSIXTime.POSIXTimeRange)
slotRangeToPosixTimeRange es ss =
  wrapContract <<< Conversion.slotRangeToPosixTimeRange es ss

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
slotToPosixTime
  :: forall (r :: Row Type)
   . EraSummariesQR
  -> SystemStartQR
  -> Slot
  -> Contract r (Either SlotToPosixTimeError POSIXTime.POSIXTime)
slotToPosixTime es ss =
  wrapContract <<< Conversion.slotToPosixTime es ss

-- https://github.com/input-output-hk/cardano-ledger/blob/2acff66e84d63a81de904e1c0de70208ff1819ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L206-L226
-- | Create an `OnchainPOSIXTimeRange` to do a round trip from an off-chain
-- | POSIXTimeRange as follows:
-- | 1) `POSIXTimeRange` -> `SlotRange`
-- | 2) `SlotRange` -> `TransactionValidity`
-- | 3) `TransactionValidity` -> `OnchainPOSIXTimeRange`
-- | `OnchainPOSIXTimeRange` is intended to equal the validity range found in
-- | the on-chain `ScriptContext`
toOnchainPosixTimeRange
  :: forall (r :: Row Type)
   . EraSummariesQR
  -> SystemStartQR
  -> POSIXTime.POSIXTimeRange
  -> Contract r
       (Either ToOnChainPosixTimeRangeError POSIXTime.OnchainPOSIXTimeRange)
toOnchainPosixTimeRange es ss =
  wrapContract <<< Conversion.toOnchainPosixTimeRange es ss

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

