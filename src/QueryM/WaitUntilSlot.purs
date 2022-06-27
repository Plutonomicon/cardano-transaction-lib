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
import Data.UInt as UInt
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Now (now)
import Helpers (liftM)
import QueryM (QueryM, getChainTip)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios (AbsSlot)
import QueryM.SystemStart (getSystemStart)
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
      | slot >= futureAbsSlotMinusPadding -> fetchRepeatedly
      | otherwise -> do
          eraSummaries <- getEraSummaries
          sysStart <- getSystemStart
          futureSlot <- liftM (error "Unable to convert AbsSlot to Slot") $
            wrap <$> UInt.fromString (BigInt.toString $ unwrap futureAbsSlot)
          futureTime <-
            liftEffect (slotToPosixTime eraSummaries sysStart futureSlot) >>=
              hush >>> liftM (error "Unable to convert Slot to POSIXTime")
          delayTime <- estimateDelayUntil futureTime
          liftAff $ delay delayTime
          fetchRepeatedly
    Chain.TipAtGenesis -> do
      -- We just retry until it moves from genesis
      liftAff $ delay retryDelay
      waitUntilSlot futureAbsSlot
  where
  -- We start `fetchRepeatedly` when the remaining time in slots is less than `timePadding`.
  timePadding :: BigInt
  timePadding = BigInt.fromInt 10

  -- Time we should aim at starting `fetchRepeatedly` at.
  futureAbsSlotMinusPadding :: AbsSlot
  futureAbsSlotMinusPadding = wrap $ unwrap futureAbsSlot - timePadding

  -- Repeatedly check current slot until it's greater than or equal to futureSlot
  fetchRepeatedly :: QueryM Chain.Tip
  fetchRepeatedly =
    getChainTip >>= case _ of
      tip@(Chain.Tip (Chain.ChainTip { slot }))
        | slot >= futureAbsSlot -> pure tip
      _ -> do
        liftAff $ delay retryDelay
        fetchRepeatedly

  retryDelay :: Milliseconds
  retryDelay = wrap 1000.0

estimateDelayUntil :: POSIXTime -> QueryM Milliseconds
estimateDelayUntil futureTimePosix = do
  futureTime <- posixTimeToMilliseconds futureTimePosix
  nowMs <- unInstant <$> liftEffect now
  pure $ wrap $ nonNegative $ unwrap futureTime - unwrap nowMs
  where
  nonNegative :: Number -> Number
  nonNegative n
    | n < 0.0 = 0.0
    | otherwise = n

posixTimeToMilliseconds :: POSIXTime -> QueryM Milliseconds
posixTimeToMilliseconds (POSIXTime futureTimeBigInt) = do
  liftEffect $ log $ show futureTimeBigInt
  liftM (error "Unable to convert POSIXTIme to Number")
    $ map (wrap <<< Int.toNumber)
    $ BigInt.toInt
    $ futureTimeBigInt / BigInt.fromInt 1000
