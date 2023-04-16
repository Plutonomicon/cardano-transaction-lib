module Ctl.Internal.Contract.WaitUntilSlot
  ( waitUntilSlot
  , waitNSlots
  , currentSlot
  , currentTime
  ) where

import Prelude

import Contract.Log (logTrace')
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Reader (asks)
import Ctl.Internal.Contract (getChainTip)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.EraSummaries (EraSummaries(EraSummaries))
import Ctl.Internal.Types.Interval
  ( POSIXTime(POSIXTime)
  , SlotToPosixTimeError(CannotFindSlotInEraSummaries)
  , findSlotEraSummary
  , getSlotLength
  , slotToPosixTime
  )
import Ctl.Internal.Types.Natural (Natural)
import Ctl.Internal.Types.Natural as Natural
import Ctl.Internal.Types.SystemStart (SystemStart)
import Data.Array (length, mapMaybe)
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds)
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Effect.Now (now)

-- | The returned slot will be no less than the slot provided as argument.
waitUntilSlot :: Slot -> Contract Chain.Tip
waitUntilSlot targetSlot = do
  queryHandle <- getQueryHandle
  { delay: retryDelay } <- asks $ _.timeParams >>> _.waitUntilSlot
  getChainTip >>= case _ of
    tip@(Chain.Tip (Chain.ChainTip { slot }))
      | slot >= targetSlot -> pure tip
      | otherwise -> do
          { systemStart } <- asks _.ledgerConstants
          eraSummaries <- liftAff $
            queryHandle.getEraSummaries
              >>= either (liftEffect <<< throw <<< show) pure
          slotLengthMs <- map getSlotLength $ liftEither
            $ lmap (const $ error "Unable to get current Era summary")
            $ findSlotEraSummary eraSummaries slot
          getLag eraSummaries systemStart slot >>= logLag slotLengthMs
          case slotToPosixTime eraSummaries systemStart targetSlot of
            Left err@(CannotFindSlotInEraSummaries _) -> do
              case latestEndSlotInEraSummaries eraSummaries of
                Nothing -> liftEffect $ throw $
                  "Unable to convert Slot to POSIXTime: " <> show err
                Just latest -> do
                  logTrace' $
                    "waitUntilSlot: Target slot is too far in the future, waiting for this slot first: "
                      <> show latest
                  -- latest - 1
                  prevLatest <-
                    liftMaybe
                      ( error
                          "waitUntilSlot: Impossible happened: negative slot"
                      ) $
                      wrap <$> BigNum.fromBigInt
                        (BigNum.toBigInt (unwrap latest) - one)
                  void $ waitUntilSlot prevLatest
                  waitUntilSlot targetSlot
            Left err -> do
              liftEffect $ throw $ "Unable to convert Slot to POSIXTime: " <>
                show err
            Right futureTime -> do
              delayTime <- estimateDelayUntil futureTime
              liftAff $ delay delayTime
              let
                -- Repeatedly check current slot until it's greater than or equal to futureAbsSlo                    t
                fetchRepeatedly :: Contract Chain.Tip
                fetchRepeatedly =
                  getChainTip >>= case _ of
                    currentTip@
                      (Chain.Tip (Chain.ChainTip { slot: currentSlot_ }))
                      | currentSlot_ >= targetSlot -> pure currentTip
                      | otherwise -> do
                          liftAff $ delay $ Milliseconds slotLengthMs
                          getLag eraSummaries systemStart currentSlot_
                            >>= logLag slotLengthMs
                          fetchRepeatedly
                    Chain.TipAtGenesis -> do
                      liftAff $ delay retryDelay
                      fetchRepeatedly
              fetchRepeatedly
    Chain.TipAtGenesis -> do
      -- We just retry until the tip moves from genesis
      liftAff $ delay retryDelay
      waitUntilSlot targetSlot
  where
  logLag :: Number -> Milliseconds -> Contract Unit
  logLag slotLengthMs (Milliseconds lag) = do
    logTrace' $
      "waitUntilSlot: current lag: " <> show lag <> " ms, "
        <> show (lag / slotLengthMs)
        <> " slots."

-- | Calculate difference between estimated POSIX time of a given slot
-- | and current time.
getLag
  :: EraSummaries
  -> SystemStart
  -> Slot
  -> Contract Milliseconds
getLag eraSummaries sysStart nowSlot = do
  nowPosixTime <- liftEither $ slotToPosixTime eraSummaries sysStart nowSlot
    # lmap (error <<< append "Unable to convert Slot to POSIXTime: " <<< show)
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  logTrace' $
    "getLag: current slot: " <> BigNum.toString (unwrap nowSlot)
      <> ", slot time: "
      <> BigInt.toString (unwrap nowPosixTime)
      <> ", system time: "
      <> show nowMs
  nowMsBigInt <- liftM (error "Unable to convert Milliseconds to BigInt") $
    BigInt.fromNumber nowMs
  pure $ wrap $ BigInt.toNumber $ nowMsBigInt - unwrap nowPosixTime

-- | Estimate how long we want to wait if we want to wait for a given
-- | `POSIXTime` starting from now.
estimateDelayUntil :: POSIXTime -> Contract Milliseconds
estimateDelayUntil futureTimePosix = do
  futureTimeSec <- posixTimeToSeconds futureTimePosix
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  let
    result = wrap $ mul 1000.0 $ nonNegative $
      unwrap futureTimeSec - nowMs / 1000.0
  logTrace' $
    "estimateDelayUntil: target time: " <> show (unwrap futureTimeSec * 1000.0)
      <> ", system time: "
      <> show nowMs
      <> ", delay: "
      <> show (unwrap result)
      <> "ms"
  pure result
  where
  nonNegative :: Number -> Number
  nonNegative n
    | n < 0.0 = 0.0
    | otherwise = n

posixTimeToSeconds :: POSIXTime -> Contract Seconds
posixTimeToSeconds (POSIXTime futureTimeBigInt) = do
  liftM (error "Unable to convert POSIXTIme to Number")
    $ map (wrap <<< Int.toNumber)
    $ BigInt.toInt
    $ futureTimeBigInt / BigInt.fromInt 1000

-- | Wait at least `offset` number of slots.
waitNSlots :: Natural -> Contract Chain.Tip
waitNSlots offset = do
  offsetBigNum <- liftM (error "Unable to convert BigInt to BigNum")
    $ (BigNum.fromBigInt <<< Natural.toBigInt) offset
  if offsetBigNum == BigNum.fromInt 0 then getChainTip
  else do
    slot <- currentSlot
    newSlot <- liftM (error "Unable to advance slot")
      $ wrap <$> BigNum.add (unwrap slot) offsetBigNum
    waitUntilSlot newSlot

currentSlot :: Contract Slot
currentSlot = getChainTip <#> case _ of
  Chain.Tip (Chain.ChainTip { slot }) -> slot
  Chain.TipAtGenesis -> (Slot <<< BigNum.fromInt) 0

-- | Get the latest POSIXTime of the current slot.
-- The plutus implementation relies on `slotToEndPOSIXTime`
-- https://github.com/input-output-hk/plutus-apps/blob/fb8a39645e532841b6e38d42ecb957f1945833a5/plutus-contract/src/Plutus/Contract/Trace.hs
currentTime :: Contract POSIXTime
currentTime = currentSlot >>= slotToEndPOSIXTime

-- | Get the ending 'POSIXTime' of a 'Slot' related to
-- | our `Contract` configuration.
-- see https://github.com/input-output-hk/plutus-apps/blob/fb8a39645e532841b6e38d42ecb957f1945833a5/plutus-ledger/src/Ledger/TimeSlot.hs
slotToEndPOSIXTime :: Slot -> Contract POSIXTime
slotToEndPOSIXTime slot = do
  targetSlot <- liftM (error "Unable to advance slot")
    $ wrap <$> BigNum.add (unwrap slot) (BigNum.fromInt 1)
  { systemStart } <- asks _.ledgerConstants
  queryHandle <- getQueryHandle
  eraSummaries <- liftAff $
    queryHandle.getEraSummaries
      >>= either (liftEffect <<< throw <<< show) pure
  futureTime <- liftEither $
    slotToPosixTime eraSummaries systemStart targetSlot
      # lmap (error <<< append "Unable to convert Slot to POSIXTime: " <<< show)
  -- We assume that a slot is 1000 milliseconds here.
  -- TODO Don't
  pure ((wrap <<< BigInt.fromInt $ -1) + futureTime)

-- | Finds the latest end slot in `EraSummaries` (if any).
latestEndSlotInEraSummaries
  :: EraSummaries
  -> Maybe Slot
latestEndSlotInEraSummaries (EraSummaries sums)
  | length sums == 0 = Nothing
  | otherwise = sums
      # mapMaybe (unwrap >>> _.end >>> map (unwrap >>> _.slot))
      # maximum
