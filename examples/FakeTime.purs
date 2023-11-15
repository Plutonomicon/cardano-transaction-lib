-- | This example demonstrates how to fake EraSummary responses to trick CTL
-- | into believing that EraSummaries span infinite time in the future (which
-- | is untrue!)
-- | Do not use this unless you know what you are doing!
module Ctl.Examples.FakeTime (contract, withInfiniteFakeTime, getSlotInOneYear) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv)
import Contract.Time
  ( EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , POSIXTime(POSIXTime)
  , PosixTimeToSlotError(CannotFindTimeInEraSummaries)
  , Slot
  , getEraSummaries
  , getSystemStart
  , posixTimeToSlot
  , slotToPosixTime
  )
import Control.Monad.Reader (local)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds, convertDuration)
import Effect.Now (now)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Test.Spec.Assertions (shouldSatisfy)

-- | This function is dangerous!
-- | It fakes EraSummary response data (makes the last epoch infinite), which
-- | may result in wrong time/slot coversions in the future.
withInfiniteFakeTime
  :: forall a
   . Warn
       ( Text
           "withInfiniteFakeTime assumes that slot lengths are never changed which is known to be untrue"
       )
  => Contract a
  -> Contract a
withInfiniteFakeTime = local patchContractEnv
  where
  patchContractEnv :: ContractEnv -> ContractEnv
  patchContractEnv env = env { handle = patchQueryHandle env.handle }
  patchQueryHandle qh =
    qh { getEraSummaries = patchGetEraSummaries qh.getEraSummaries }
  patchGetEraSummaries getEraSummaries = do
    getEraSummaries <#> map
      ( over EraSummaries $ \summaries -> Array.slice 0 (-1) summaries <>
          foldMap makeInfinite
            (Array.last summaries)
      )

  makeInfinite :: EraSummary -> Array EraSummary
  makeInfinite (EraSummary sum) = [ EraSummary sum { end = Nothing } ]

getSlotInOneYear :: Contract (Either PosixTimeToSlotError Slot)
getSlotInOneYear = do
  logInfo' "Running Examples.Pkh2Pkh"
  systemStart <- getSystemStart
  eraSummaries <- getEraSummaries
  nowTime :: Milliseconds <- liftEffect $ convertDuration <<< unInstant <$> now
  logInfo' $ show $ unwrap nowTime
  pure $ posixTimeToSlot eraSummaries systemStart
    ( POSIXTime
        ( unsafePartial $ fromJust $ BigInt.fromNumber $ unwrap nowTime +
            oneYear
        )
    )
  where
  oneYear = 1000.0 * 3600.0 * 24.0 * 365.0

contract :: Contract Unit
contract = do
  getSlotInOneYear >>= flip shouldSatisfy \x -> case x of
    Left (CannotFindTimeInEraSummaries _) -> true
    _ -> false
  withInfiniteFakeTime do
    getSlotInOneYear >>= logInfo' <<< show
    getSlotInOneYear >>= flip shouldSatisfy isRight
    getSlotInOneYear >>= traverse_ \slot -> do
      systemStart <- getSystemStart
      eraSummaries <- getEraSummaries
      logInfo' $ show $ slotToPosixTime eraSummaries systemStart slot
      slotToPosixTime eraSummaries systemStart slot `shouldSatisfy` isRight
