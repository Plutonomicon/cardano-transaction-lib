module Test.Ctl.BalanceTx.Time (suite) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, runContract)
import Contract.ScriptLookups
  ( ScriptLookups
  , UnattachedUnbalancedTx
  , mkUnbalancedTx
  )
import Contract.Time
  ( EraSummaries
  , Interval
  , POSIXTime
  , Slot
  , SystemStart
  , mkFiniteInterval
  , slotToPosixTime
  )
import Contract.TxConstraints (TxConstraints, mustValidateIn)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Ctl.Internal.Cardano.Types.Transaction
  ( _body
  , _ttl
  , _validityStartInterval
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.UnbalancedTransaction (_transaction)
import Data.BigInt (fromInt) as BigInt
import Data.Lens (view)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Mote (group, test)
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

startTime :: POSIXTime
startTime = wrap $ BigInt.fromInt 100000

finiteTimeRange1 :: Interval POSIXTime
finiteTimeRange1 = mkFiniteInterval startTime (startTime + startTime)

emptyLookup :: ScriptLookups Void
emptyLookup = mempty

getTimeFromUnbalanced
  :: UnattachedUnbalancedTx -> Effect (Maybe POSIXTime /\ Maybe POSIXTime)
getTimeFromUnbalanced utx =
  let
    body = (_transaction <<< _body) `view` (unwrap utx).unbalancedTx
    timeToLive = _ttl `view` body
    validityStart = _validityStartInterval `view` body
  in
    do
      eraSummaries <- eraSummariesFixture
      systemStart <- systemStartFixture
      let toPosix = toPosixTime eraSummaries systemStart
      (/\) <$> traverse toPosix timeToLive <*> traverse toPosix validityStart
  where
  toPosixTime :: EraSummaries -> SystemStart -> Slot -> Effect POSIXTime
  toPosixTime eraSummaries systemStart time = do
    eitherTime <- slotToPosixTime eraSummaries systemStart time
    case eitherTime of
      Left e -> (throwError <<< error <<< show) e
      Right value -> pure value

oneTimeConstrainTest :: Contract () Unit
oneTimeConstrainTest = mkUnbalancedTx emptyLookup constraint >>=
  case _ of
    Left e -> fail $ show e
    Right utx ->
      do
        mtimeToLive /\ mvalidityStart <- liftEffect $ getTimeFromUnbalanced utx
        timeToLive <- liftMaybe (error "Expected a time to live") mtimeToLive
        validityStart <- liftMaybe
          (error "Expected a validity start time")
          mvalidityStart
        startTime `shouldEqual` validityStart
        (startTime + startTime) `shouldEqual` timeToLive

  where
  constraint :: TxConstraints Void Void
  constraint = mustValidateIn finiteTimeRange1

