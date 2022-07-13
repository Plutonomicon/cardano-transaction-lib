module QueryM.WaitUntilSlot
  ( waitUntilSlot
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.DateTime.Instant (unInstant)
import Data.Either (hush)
import Data.Int as Int
import Data.Log.Level (LogLevel(Trace))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds)
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Helpers (liftEither, liftM, logString)
import QueryM (QueryM, getChainTip)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios (EraSummaries, SystemStart)
import QueryM.SystemStart (getSystemStart)
import Serialization.Address (Slot)
import Types.BigNum as BigNum
import Types.Chain as Chain
import Types.Interval
  ( POSIXTime(POSIXTime)
  , findSlotEraSummary
  , getSlotLength
  , slotToPosixTime
  )

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
                currentTip@(Chain.Tip (Chain.ChainTip { slot: currentSlot }))
                  | currentSlot >= futureSlot -> pure currentTip
                  | otherwise -> do
                      liftAff $ delay $ Milliseconds $ BigInt.toNumber
                        slotLengthMs
                      getLag eraSummaries sysStart currentSlot >>= logLag
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

  logLag :: BigInt -> Milliseconds -> QueryM Unit
  logLag slotLengthMs (Milliseconds lag) = do
    logLevel <- asks $ _.config >>> _.logLevel
    liftEffect $ logString logLevel Trace $
      "waitUntilSlot: current lag: " <> show lag <> " ms, "
        <> show (lag / BigInt.toNumber slotLengthMs)
        <> " slots."

-- | Calculate difference between estimated POSIX time of given slot
-- | and current time.
getLag :: EraSummaries -> SystemStart -> Slot -> QueryM Milliseconds
getLag eraSummaries sysStart nowSlot = do
  logLevel <- asks $ _.config >>> _.logLevel
  nowPosixTime <- liftEffect (slotToPosixTime eraSummaries sysStart nowSlot) >>=
    hush >>> liftM (error "Unable to convert Slot to POSIXTime")
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  liftEffect $ logString logLevel Trace $
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
  logLevel <- asks $ _.config >>> _.logLevel
  let
    result = wrap $ mul 1000.0 $ nonNegative $
      unwrap futureTimeSec - nowMs / 1000.0
  liftEffect $ logString logLevel Trace $
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
