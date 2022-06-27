module QueryM.WaitUntilSlot
  ( waitUntilSlot
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.DateTime.Instant (unInstant)
import Data.Either (hush)
import Data.Int as Int
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(..))
import Data.UInt as UInt
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Now (now)
import Helpers (liftM)
import QueryM (QueryM, getChainTip)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios (AbsSlot, EraSummaries(..), SystemStart(..))
import QueryM.SystemStart (getSystemStart)
import Serialization.Address (Slot(..))
import Types.Chain as Chain
import Types.Interval (POSIXTime(..), slotToPosixTime)

-- | Wait until slot. The returned slot will be no less than the slot provided
-- | as argument. The returned slot may be greater than the real current slot of
-- | the node.
waitUntilSlot :: AbsSlot -> QueryM Chain.Tip
waitUntilSlot futureAbsSlot =
  getChainTip >>= case _ of
    tip@(Chain.Tip (Chain.ChainTip { slot }))
      | slot >= futureAbsSlot -> pure tip
      | otherwise -> do
          eraSummaries <- getEraSummaries
          sysStart <- getSystemStart
          let
            -- Repeatedly check current slot until it's greater than or equal to futureSlot
            fetchRepeatedly :: QueryM Chain.Tip
            fetchRepeatedly =
              getChainTip >>= case _ of
                tip@(Chain.Tip (Chain.ChainTip { slot }))
                  | slot >= futureAbsSlot -> pure tip
                _ -> do
                  liftAff $ delay retryDelay
                  lag <- getLag eraSummaries sysStart slot
                  liftEffect $ Console.log $ "lag: " <> show lag
                  fetchRepeatedly
          lag <- getLag eraSummaries sysStart slot
          liftEffect $ Console.log $ "lag: " <> show lag
          futureSlot <- liftM (error "Unable to convert AbsSlot to Slot") $
            wrap <$> UInt.fromString (BigInt.toString $ unwrap futureAbsSlot)
          futureTime <-
            liftEffect (slotToPosixTime eraSummaries sysStart futureSlot) >>=
              hush >>> liftM (error "Unable to convert Slot to POSIXTime")
          delayTime <- estimateDelayUntil futureTime
          liftEffect $ Console.log $ "delaying for" <> show delayTime
          liftAff $ delay delayTime
          fetchRepeatedly
    Chain.TipAtGenesis -> do
      -- We just retry until it moves from genesis
      liftAff $ delay retryDelay
      waitUntilSlot futureAbsSlot
  where
  retryDelay :: Milliseconds
  retryDelay = wrap 1000.0

getLag :: EraSummaries -> SystemStart -> AbsSlot -> QueryM BigInt
getLag eraSummaries sysStart nowAbsSlot = do
  nowSlot <- liftM (error "Unable to convert AbsSlot to Slot") $
    wrap <$> UInt.fromString (BigInt.toString $ unwrap nowAbsSlot)
  liftEffect $ Console.log $ "nowSlot: " <> show nowSlot
  nowPosixTime <- liftEffect (slotToPosixTime eraSummaries sysStart nowSlot) >>=
    hush >>> liftM (error "Unable to convert Slot to POSIXTime")
  liftEffect $ Console.log $ "nowPosixTime: " <> show nowPosixTime
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  liftEffect $ Console.log $ "nowMs: " <> show nowMs
  nowMsBigInt <- liftM (error "Unable to convert Milliseconds to BigInt") $
    BigInt.fromNumber nowMs
  pure $ nowMsBigInt - unwrap nowPosixTime

-- | Estimate how long we want to wait if we want to wait until `timePadding`
-- | milliseconds before a given `POSIXTime`.
estimateDelayUntil :: POSIXTime -> QueryM Milliseconds
estimateDelayUntil futureTimePosix = do
  futureTimeSec <- posixTimeToSeconds futureTimePosix
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  liftEffect $ Console.log $ show nowMs
  liftEffect $ Console.log $ show futureTimeSec
  let
    result = wrap $ mul 1000.0 $ nonNegative $
      unwrap futureTimeSec - nowMs / 1000.0 - unwrap timePaddingSec
  pure result
  where
  -- We start `fetchRepeatedly` when the remaining time in seconds is less than `timePadding`.
  timePaddingSec :: Seconds
  timePaddingSec = wrap 50.0

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
