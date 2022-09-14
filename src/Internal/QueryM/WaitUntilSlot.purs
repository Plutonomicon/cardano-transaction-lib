module CTL.Internal.QueryM.WaitUntilSlot
  ( waitUntilSlot
  , waitNSlots
  , currentSlot
  , currentTime
  ) where

import Prelude

import CTL.Contract.Log (logTrace')
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.DateTime.Instant (unInstant)
import Data.Either (hush)
import Data.Int as Int
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds)
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import CTL.Internal.Helpers (liftEither, liftM)
import CTL.Internal.QueryM (QueryM, getChainTip)
import CTL.Internal.QueryM.EraSummaries (getEraSummaries)
import CTL.Internal.QueryM.Ogmios (EraSummaries, SystemStart)
import CTL.Internal.QueryM.SystemStart (getSystemStart)
import CTL.Internal.Serialization.Address (Slot(Slot))
import CTL.Internal.Types.BigNum as BigNum
import CTL.Internal.Types.Chain as Chain
import CTL.Internal.Types.Interval
  ( POSIXTime(POSIXTime)
  , findSlotEraSummary
  , getSlotLength
  , slotToPosixTime
  )
import CTL.Internal.Types.Natural (Natural)
import CTL.Internal.Types.Natural as Natural

-- | The returned slot will be no less than the slot provided as argument.
waitUntilSlot :: Slot -> QueryM Chain.Tip
waitUntilSlot futureSlot =
  getChainTip >>= case _ of
    tip@(Chain.Tip (Chain.ChainTip { slot }))
      | slot >= futureSlot -> pure tip
      | otherwise -> do
          eraSummaries <- getEraSummaries
          sysStart <- getSystemStart
          slotLengthMs <- map getSlotLength $ liftEither
            $ lmap (const $ error "Unable to get current Era summary")
            $ findSlotEraSummary eraSummaries slot
          -- `timePadding` in slots
          -- If there are less than `slotPadding` slots remaining, start querying for chainTip
          -- repeatedly, because it's possible that at any given moment Ogmios suddenly
          -- synchronizes with node that is also synchronized with global time.
          getLag eraSummaries sysStart slot >>= logLag slotLengthMs
          futureTime <-
            liftEffect (slotToPosixTime eraSummaries sysStart futureSlot)
              >>= hush >>> liftM (error "Unable to convert Slot to POSIXTime")
          delayTime <- estimateDelayUntil futureTime
          liftAff $ delay delayTime
          let
            -- Repeatedly check current slot until it's greater than or equal to futureAbsSlot
            fetchRepeatedly :: QueryM Chain.Tip
            fetchRepeatedly =
              getChainTip >>= case _ of
                currentTip@(Chain.Tip (Chain.ChainTip { slot: currentSlot_ }))
                  | currentSlot_ >= futureSlot -> pure currentTip
                  | otherwise -> do
                      liftAff $ delay $ Milliseconds slotLengthMs
                      getLag eraSummaries sysStart currentSlot_ >>= logLag
                        slotLengthMs
                      fetchRepeatedly
                Chain.TipAtGenesis -> do
                  liftAff $ delay retryDelay
                  fetchRepeatedly
          fetchRepeatedly
    Chain.TipAtGenesis -> do
      -- We just retry until the tip moves from genesis
      liftAff $ delay retryDelay
      waitUntilSlot futureSlot
  where
  retryDelay :: Milliseconds
  retryDelay = wrap 1000.0

  logLag :: Number -> Milliseconds -> QueryM Unit
  logLag slotLengthMs (Milliseconds lag) = do
    logTrace' $
      "waitUntilSlot: current lag: " <> show lag <> " ms, "
        <> show (lag / slotLengthMs)
        <> " slots."

-- | Calculate difference between estimated POSIX time of given slot
-- | and current time.
getLag :: EraSummaries -> SystemStart -> Slot -> QueryM Milliseconds
getLag eraSummaries sysStart nowSlot = do
  nowPosixTime <- liftEffect (slotToPosixTime eraSummaries sysStart nowSlot) >>=
    hush >>> liftM (error "Unable to convert Slot to POSIXTime")
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

-- | Estimate how long we want to wait if we want to wait until `timePadding`
-- | milliseconds before a given `POSIXTime`.
estimateDelayUntil :: POSIXTime -> QueryM Milliseconds
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

posixTimeToSeconds :: POSIXTime -> QueryM Seconds
posixTimeToSeconds (POSIXTime futureTimeBigInt) = do
  liftM (error "Unable to convert POSIXTIme to Number")
    $ map (wrap <<< Int.toNumber)
    $ BigInt.toInt
    $ futureTimeBigInt / BigInt.fromInt 1000

-- | Wait at least `offset` number of slots.
waitNSlots :: Natural -> QueryM Chain.Tip
waitNSlots offset = do
  offsetBigNum <- liftM (error "Unable to convert BigInt to BigNum")
    $ (BigNum.fromBigInt <<< Natural.toBigInt) offset
  if offsetBigNum == BigNum.fromInt 0 then getChainTip
  else do
    slot <- currentSlot
    newSlot <- liftM (error "Unable to advance slot")
      $ wrap <$> BigNum.add (unwrap slot) offsetBigNum
    waitUntilSlot newSlot

currentSlot :: QueryM Slot
currentSlot = getChainTip <#> case _ of
  Chain.Tip (Chain.ChainTip { slot }) -> slot
  Chain.TipAtGenesis -> (Slot <<< BigNum.fromInt) 0

-- | Get the latest POSIXTime of the current slot.
-- The plutus implementation relies on `slotToEndPOSIXTime`
-- https://github.com/input-output-hk/plutus-apps/blob/fb8a39645e532841b6e38d42ecb957f1945833a5/plutus-contract/src/Plutus/Contract/Trace.hs
currentTime :: QueryM POSIXTime
currentTime = currentSlot >>= slotToEndPOSIXTime

-- | Get the ending 'POSIXTime' of a 'Slot' related to
-- | our `QueryM` configuration.
-- see https://github.com/input-output-hk/plutus-apps/blob/fb8a39645e532841b6e38d42ecb957f1945833a5/plutus-ledger/src/Ledger/TimeSlot.hs
slotToEndPOSIXTime :: Slot -> QueryM POSIXTime
slotToEndPOSIXTime slot = do
  futureSlot <- liftM (error "Unable to advance slot")
    $ wrap <$> BigNum.add (unwrap slot) (BigNum.fromInt 1)
  eraSummaries <- getEraSummaries
  sysStart <- getSystemStart
  futureTime <- liftEffect $ slotToPosixTime eraSummaries sysStart futureSlot
    >>= hush >>> liftM (error "Unable to convert Slot to POSIXTime")
  -- We assume that a slot is 1000 milliseconds here.
  pure ((wrap <<< BigInt.fromInt $ -1) + futureTime)
