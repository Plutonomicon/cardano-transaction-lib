-- | A module containing time-related datatypes and helpers.
module Contract.Time
  ( getCurrentEpoch
  , getEraSummaries
  , getSystemStart
  , getCurrentEra
  , normalizeTimeInterval
  , module Chain
  , module ExportEraSummaries
  , module ExportOgmios
  , module ExportSystemStart
  , module Interval
  , module SerializationAddress
  ) where

import Prelude

import Contract.Chain (getTip) as Chain
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Cardano.Types.Transaction (Epoch(Epoch))
import Ctl.Internal.Contract (getChainTip)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.QueryM.Ogmios (CurrentEpoch(CurrentEpoch))
import Ctl.Internal.QueryM.Ogmios
  ( CurrentEpoch(CurrentEpoch)
  , OgmiosEraSummaries(OgmiosEraSummaries)
  ) as ExportOgmios
import Ctl.Internal.Serialization.Address (BlockId(BlockId), Slot(Slot)) as SerializationAddress
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Types.Chain
  ( BlockHeaderHash(BlockHeaderHash)
  , ChainTip(ChainTip)
  , Tip(TipAtGenesis, Tip)
  ) as Chain
import Ctl.Internal.Types.EraSummaries
  ( EpochLength(EpochLength)
  , EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , SafeZone(SafeZone)
  , SlotLength
  ) as ExportEraSummaries
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries
  , EraSummary
  )
import Ctl.Internal.Types.Interval
  ( AbsTime(AbsTime)
  , Closure
  , Extended(NegInf, Finite, PosInf)
  , Interval(FiniteInterval)
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
import Ctl.Internal.Types.SystemStart (SystemStart)
import Ctl.Internal.Types.SystemStart (SystemStart(SystemStart)) as ExportSystemStart
import Data.Array as Array
import Data.Foldable (find)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.UInt as UInt
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import JS.BigInt as BigInt

-- | Get a summary of the current era.
getCurrentEra :: Contract EraSummary
getCurrentEra = do
  eraSummaries <- getEraSummaries
  currentSlot <- getCurrentSlot
  logInfo' $ "getCurrentEra: era summaries: " <> show eraSummaries
  logInfo' $ "getCurrentEra: current slot: " <> show currentSlot
  liftContractM "getCurrentEra: Could not find era summary"
    $ find (slotInRange currentSlot)
    $ Array.sortBy (comparing getStartSlot)
    $ unwrap eraSummaries
  where
  getStartSlot :: EraSummary -> Slot
  getStartSlot = unwrap >>> _.start >>> unwrap >>> _.slot

  slotInRange :: Slot -> EraSummary -> Boolean
  slotInRange currentSlot era =
    let
      eraStartSlot = getStartSlot era
      startNotAfterUs = eraStartSlot <= currentSlot
    in
      case era # unwrap # _.end of
        Nothing -> startNotAfterUs
        Just eraEnd -> startNotAfterUs &&
          ( (eraEnd # unwrap # _.slot) >
              currentSlot
          )

  getCurrentSlot :: Contract Slot
  getCurrentSlot = do
    { delay: delayMs } <- asks $ _.timeParams >>> _.awaitTxConfirmed
    getChainTip >>= case _ of
      Chain.TipAtGenesis -> do
        liftAff $ delay delayMs
        getCurrentSlot
      Chain.Tip (Chain.ChainTip { slot }) -> pure slot

-- | Given a desired range, tighten it to fit onchain.
normalizeTimeInterval
  :: Interval.Interval Interval.POSIXTime
  -> Contract (Interval.Interval Interval.POSIXTime)
normalizeTimeInterval = case _ of
  desired@(Interval.FiniteInterval start end) -> do
    era <- getCurrentEra
    let
      params = unwrap (unwrap era).parameters
      slotLength = unwrap params.slotLength
    let offset = unwrap params.safeZone + slotLength
    let endTime = start + Interval.POSIXTime offset
    let oneSecond = Interval.POSIXTime $ BigInt.fromInt 1_000
    let
      range = Interval.FiniteInterval (start + oneSecond)
        (min end (endTime - oneSecond))
    logInfo' $ "normalizeTimeInterval: desired range: " <> show desired
    logInfo' $ "normalizeTimeInterval: computed range: " <> show range
    pure range
  i -> liftContractM
    ("normalizeTimeInterval: could not convert to start-end range: " <> show i)
    Nothing

-- | Get the current Epoch.
getCurrentEpoch :: Contract Epoch
getCurrentEpoch = do
  queryHandle <- getQueryHandle
  CurrentEpoch bigInt <- liftAff $ queryHandle.getCurrentEpoch
  map Epoch $ liftM (error "Unable to convert CurrentEpoch")
    $ UInt.fromString
    $ BigInt.toString (bigInt :: BigInt.BigInt)

-- | Get `EraSummaries` as used for Slot arithemetic.
-- |
-- | More info can be found in Ogmios or Blockfrost docs (see links below).
-- | Currently we use the same data type definition.
-- | https://ogmios.dev/api/ under "eraSummaries" query
-- | https://docs.blockfrost.io/#tag/Cardano-Network/paths/~1network~1eras/get
getEraSummaries :: Contract EraSummaries
getEraSummaries = do
  queryHandle <- getQueryHandle
  liftedE $ liftAff $ queryHandle.getEraSummaries

-- | Get the current system start time.
getSystemStart :: Contract SystemStart
getSystemStart = asks $ _.ledgerConstants >>> _.systemStart
