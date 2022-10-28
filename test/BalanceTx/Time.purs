module Test.Ctl.BalanceTx.Time (suite) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract)
import Contract.ScriptLookups
  ( ScriptLookups
  , UnattachedUnbalancedTx
  , mkUnbalancedTx
  )
import Contract.Time
  ( POSIXTime
  , Slot
  , always
  , from
  , maxSlot
  , mkFiniteInterval
  , never
  , posixTimeToSlot
  , slotToPosixTime
  , to
  )
import Contract.TxConstraints (mustValidateIn)
import Control.Monad.Except (throwError)
import Ctl.Internal.Cardano.Types.Transaction (_body)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Interval (Interval)
import Ctl.Internal.Types.UnbalancedTransaction (_transaction)
import Data.BigInt (fromString) as BigInt
import Data.Lens (view)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Types.Interval (eraSummariesFixture, systemStartFixture)
import Test.Spec.Assertions (fail, shouldEqual)

-- halt if wrong time inputs 
-- set one time constraint 
-- set multiple time contraints

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "BalanceTx.Time"
    $ test "simple time constraint"
    $ run oneTimeConstrainTest

  where
  run = runContract testnetConfig { suppressLogs = true }

emptyLookup :: ScriptLookups Void
emptyLookup = mempty

now :: POSIXTime
now = mkPosixTime "1666918454999"

getTimeFromUnbalanced
  :: UnattachedUnbalancedTx -> Effect (Interval POSIXTime)
getTimeFromUnbalanced utx =
  validityToPosixTime (unwrap body)
  where
  body = (_transaction <<< _body) `view` (unwrap utx).unbalancedTx

toPosixTime :: Slot -> Effect POSIXTime
toPosixTime time = do
  eraSummaries <- eraSummariesFixture
  systemStart <- systemStartFixture
  eitherTime <- slotToPosixTime eraSummaries systemStart time
  case eitherTime of
    Left e -> (throwError <<< error <<< show) e
    Right value -> pure value

toSlot :: POSIXTime -> Effect Slot
toSlot time = do
  eraSummaries <- eraSummariesFixture
  systemStart <- systemStartFixture
  eitherTime <- posixTimeToSlot eraSummaries systemStart time
  case eitherTime of
    Left e -> (throwError <<< error <<< show) e
    Right value -> pure value

validityToPosixTime
  :: forall (r :: Row Type)
   . { validityStartInterval :: Maybe Slot, ttl :: Maybe Slot | r }
  -> Effect (Interval POSIXTime)
validityToPosixTime { validityStartInterval, ttl: timeToLive } =
  case validityStartInterval of
    Just start ->
      if start == maxSlot then
        pure never
      else
        case timeToLive of
          Just end ->
            do
              start' <- toPosixTime start
              preEnd <- toPosixTime end
              pure $ mkFiniteInterval start' (preEnd `add` (negate one))
          Nothing ->
            from <$> toPosixTime start
    Nothing ->
      case timeToLive of
        Nothing -> pure always
        Just end -> to <$> toPosixTime end

mkPosixTime :: String -> POSIXTime
mkPosixTime = wrap <<< unsafePartial fromJust <<< BigInt.fromString

oneTimeConstrainTest :: Contract () Unit
oneTimeConstrainTest = do
  let
    interval = mkFiniteInterval now now
    constraint = mustValidateIn interval
  mutx <- mkUnbalancedTx emptyLookup constraint
  now2 <- liftEffect $ toSlot now
  logInfo' $ show now2
  logInfo' $ show mutx
  case mutx of
    Left e -> fail $ show e
    Right utx ->
      do
        returnedInterval <- liftEffect $ getTimeFromUnbalanced utx
        returnedInterval `shouldEqual` interval

