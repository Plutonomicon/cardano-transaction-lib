-- | A module containing time-related datatypes and helpers.
module Contract.Time
  ( getCurrentEpoch
  , getEraSummaries
  , getSystemStart
  , module Chain
  , module ExportOgmios
  , module Interval
  , module SerializationAddress
  ) where

import Prelude

import Contract.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(Tip, TipAtGenesis)
  , getTip
  ) as Chain
import Contract.Monad (Contract)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Cardano.Types.Transaction (Epoch(Epoch))
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries) as EraSummaries
import Ctl.Internal.QueryM.Ogmios
  ( CurrentEpoch(CurrentEpoch)
  , EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , RelativeTime(RelativeTime)
  , SafeZone(SafeZone)
  , SlotLength(SlotLength)
  , SystemStart(SystemStart)
  ) as ExportOgmios
import Ctl.Internal.QueryM.Ogmios
  ( CurrentEpoch(CurrentEpoch)
  , EraSummaries
  , SystemStart
  )
import Ctl.Internal.Serialization.Address (BlockId(BlockId), Slot(Slot)) as SerializationAddress
import Ctl.Internal.Types.Interval
  ( AbsTime(AbsTime)
  , Closure
  , Extended(NegInf, Finite, PosInf)
  , Interval
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
  , isEmpty
  , lowerBound
  , maxSlot
  , member
  , mkFiniteInterval
  , never
  , overlaps
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
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)

-- | Get the current Epoch.
getCurrentEpoch :: Contract Epoch
getCurrentEpoch = do
  queryHandle <- getQueryHandle
  CurrentEpoch bigInt <- liftAff $ queryHandle.getCurrentEpoch
  map Epoch $ liftM (error "Unable to convert CurrentEpoch")
    $ UInt.fromString
    $ BigInt.toString (bigInt :: BigInt.BigInt)

-- | Get `EraSummaries` as used for Slot arithemetic.
-- | Details can be found https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: Contract EraSummaries
getEraSummaries = wrapQueryM EraSummaries.getEraSummaries

-- | Get the current system start time.
getSystemStart :: Contract SystemStart
getSystemStart =
  asks $ _.ledgerConstants >>> _.systemStart
