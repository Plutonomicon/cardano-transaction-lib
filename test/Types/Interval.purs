module Test.Types.Interval
  ( suite
  , eraSummariesFixture
  , systemStartFixture
  ) where

import Prelude

import Aeson (decodeJsonString)
import Control.Monad.Except (throwError)
import Data.BigInt (fromString) as BigInt
import Data.Const (Const)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (fromJust)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Mote (MoteT, group, test)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import QueryM.Ogmios (EraSummaries, SystemStart)
import Serialization.Address (Slot(Slot))
import Test.Spec.Assertions (shouldEqual)
import Types.BigNum (fromInt) as BigNum
import Types.Interval
  ( PosixTimeToSlotError(PosixTimeBeforeSystemStart)
  , POSIXTime(POSIXTime)
  , posixTimeToSlot
  , slotToPosixTime
  )

type TestPlanM a = MoteT (Const Void)
  (EraSummaries -> SystemStart -> Effect Unit)
  Aff
  a

suite :: TestPlanM Unit
suite = do
  group "Interval type" do
    test "Inverse posixTimeToSlot >>> slotToPosixTime " $ testPosixTimeToSlot
    test "Inverse slotToPosixTime >>> posixTimeToSlot " $ testSlotToPosixTime
    test "PosixTimeToSlot errors" $ testPosixTimeToSlotError

-- launchAff_ $ runContract testnetConfig $ getEraSummaries >>= encodeAeson >>> stringifyAeson >>> logShow
eraSummariesFixture :: EraSummaries
eraSummariesFixture =
  either (\_ -> unsafeCrashWith "internal error") identity $ decodeJsonString
    "[{\"end\":{\"epoch\":74,\"slot\":1598400,\"time\":31968000},\"parameters\":{\"epochLength\":21600,\"safeZone\":4320,\"slotLength\":20},\"start\":{\"epoch\":0,\"slot\":0,\"time\":0}},{\"end\":{\"epoch\":102,\"slot\":13694400,\"time\":44064000},\"parameters\":{\"epochLength\":432000,\"safeZone\":129600,\"slotLength\":1},\"start\":{\"epoch\":74,\"slot\":1598400,\"time\":31968000}},{\"end\":{\"epoch\":112,\"slot\":18014400,\"time\":48384000},\"parameters\":{\"epochLength\":432000,\"safeZone\":129600,\"slotLength\":1},\"start\":{\"epoch\":102,\"slot\":13694400,\"time\":44064000}},{\"end\":{\"epoch\":154,\"slot\":36158400,\"time\":66528000},\"parameters\":{\"epochLength\":432000,\"safeZone\":129600,\"slotLength\":1},\"start\":{\"epoch\":112,\"slot\":18014400,\"time\":48384000}},{\"end\":{\"epoch\":215,\"slot\":62510400,\"time\":92880000},\"parameters\":{\"epochLength\":432000,\"safeZone\":129600,\"slotLength\":1},\"start\":{\"epoch\":154,\"slot\":36158400,\"time\":66528000}},{\"end\":{\"epoch\":224,\"slot\":66398400,\"time\":96768000},\"parameters\":{\"epochLength\":432000,\"safeZone\":129600,\"slotLength\":1},\"start\":{\"epoch\":215,\"slot\":62510400,\"time\":92880000}}]"

-- launchAff_ $ runContract testnetConfig $ getSystemStart >>= encodeAeson >>> stringifyAeson >>> logShow
systemStartFixture :: SystemStart
systemStartFixture = either (\_ -> unsafeCrashWith "internal error") identity $
  decodeJsonString "\"2019-07-24T20:20:16Z\""

testPosixTimeToSlot :: EraSummaries -> SystemStart -> Effect Unit
testPosixTimeToSlot eraSummaries sysStart = do
  let
    -- Tests currently pass "exactly" for seconds precision, which makes sense
    -- given converting to a Slot will round down to the near slot length
    -- (mostly 1s). If it rounds down and is the end slot, then a check is in
    -- place that any "extra" time is zero.
    -- We can allow for Millseconds as (off chain) input if we assume
    -- the seconds provided by Ogmios are exact, which seems to be the case
    -- here https://cardano.stackexchange.com/questions/7034/how-to-convert-posixtime-to-slot-number-on-cardano-testnet/7035#7035
    -- `timeWhenSlotChangedTo1Sec = POSIXTime 1595967616000` - exactly
    -- divisible by 1 second.

    -- *Testing far into the future note during hardforks:*
    -- It's worth noting that testing values "in" the recent era summary may
    -- fail during hardforks. This is because the last element's `end`
    -- field may be non null, meaning there is a limit to how far we can go
    -- into the future for reliable slot/time conversion (an exception like
    -- `CannotFindSlotInEraSummaries` is raised in this case).
    -- This `end` field presumably changes to `null` after the the initial
    -- period is over and things stabilise.
    -- For example, at the time of writing (start of Vasil hardfork during
    -- Babbage era), the *last* era summary element is
    -- ```
    -- {
    --   "start": {
    --     "time": 92880000,
    --     "slot": 62510400,
    --     "epoch": 215
    --   },
    --   "end": {
    --     "time": 93312000,
    --     "slot": 62942400,
    --     "epoch": 216
    --   },
    --   "parameters": {
    --     "epochLength": 432000,
    --     "slotLength": 1,
    --     "safeZone": 129600
    --   }
    -- }
    -- ```
    -- Note, `end` isn't null. This means any time "after" 93312000 will raise
    -- `CannotFindSlotInEraSummaries` an exception. So adding a time far
    -- into the future for `posixTimes` below will raise this exception.
    -- Notice also how 93312000 - 92880000 is a relatively small period of
    -- time so I expect this will change to `null` once things stabilise.
    posixTimes = mkPosixTime <$>
      [ "1603636353000"
      , "1613636755000"
      ]
  traverse_ (idTest eraSummaries sysStart identity) posixTimes
  -- With Milliseconds, we generally round down, provided the aren't at the
  -- end  with non-zero excess:
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1613636754000")
    (mkPosixTime "1613636754999")
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1613636754000")
    (mkPosixTime "1613636754500")
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1613636754000")
    (mkPosixTime "1613636754499")
  where
  idTest
    :: EraSummaries
    -> SystemStart
    -> (POSIXTime -> POSIXTime)
    -> POSIXTime
    -> Effect Unit
  idTest es ss transf posixTime = do
    posixTimeToSlot es ss posixTime >>= case _ of
      Left err -> throwError $ error $ show err
      Right slot -> do
        ePosixTime <- slotToPosixTime es ss slot
        either (throwError <<< error <<< show) (shouldEqual $ transf posixTime)
          ePosixTime

mkPosixTime :: String -> POSIXTime
mkPosixTime = POSIXTime <<< unsafePartial fromJust <<< BigInt.fromString

testSlotToPosixTime :: EraSummaries -> SystemStart -> Effect Unit
testSlotToPosixTime eraSummaries sysStart = do
  -- See *Testing far into the future note during hardforks:* for details on
  -- how far into the future we test with slots when a hardfork occurs.
  let
    slots = mkSlot <$>
      [ 58278567
      , 48272312
      , 39270783
      , 957323
      , 34952
      , 7532
      , 232
      , 1
      ]
  traverse_ (idTest eraSummaries sysStart) slots
  where
  idTest :: EraSummaries -> SystemStart -> Slot -> Effect Unit
  idTest es ss slot = do
    slotToPosixTime es ss slot >>= case _ of
      Left err -> throwError $ error $ show err
      Right posixTime -> do
        eSlot <- posixTimeToSlot es ss posixTime
        either (throwError <<< error <<< show) (shouldEqual slot) eSlot

  mkSlot :: Int -> Slot
  mkSlot = Slot <<< BigNum.fromInt

testPosixTimeToSlotError :: EraSummaries -> SystemStart -> Effect Unit
testPosixTimeToSlotError eraSummaries sysStart = do
  let
    posixTime = mkPosixTime "1000"
  -- Some difficulty reproducing all the errors
  errTest eraSummaries sysStart
    posixTime
    (PosixTimeBeforeSystemStart posixTime)
  where
  errTest
    :: forall (err :: Type)
     . EraSummaries
    -> SystemStart
    -> POSIXTime
    -> PosixTimeToSlotError
    -> Effect Unit
  errTest es ss posixTime expectedErr = do
    posixTimeToSlot es ss posixTime >>= case _ of
      Left err -> err `shouldEqual` expectedErr
      Right _ ->
        throwError $ error $ "Test should have failed giving: " <> show
          expectedErr
