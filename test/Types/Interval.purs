module Test.Ctl.Types.Interval
  ( suite
  , eraSummariesFixture
  , systemStartFixture
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString, printJsonDecodeError)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (throwError)
import Ctl.Internal.QueryM.Ogmios (OgmiosEraSummaries, OgmiosSystemStart)
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.BigNum (fromInt) as BigNum
import Ctl.Internal.Types.EraSummaries (EraSummaries)
import Ctl.Internal.Types.Interval
  ( Interval
  , POSIXTime(POSIXTime)
  , PosixTimeToSlotError(PosixTimeBeforeSystemStart)
  , always
  , contains
  , from
  , hull
  , intersection
  , isEmpty
  , member
  , mkFiniteInterval
  , never
  , posixTimeToSlot
  , slotToPosixTime
  , to
  )
import Ctl.Internal.Types.SystemStart (SystemStart)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Exception (error)
import JS.BigInt (fromInt, fromString) as BigInt
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (concat) as Path
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result(Success, Failed), quickCheck, (<?>))
import Test.QuickCheck.Combinators ((&=&))
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (EraSummaries -> SystemStart -> Effect Unit) Unit
suite = do
  group "Interval" do
    group "EraSumaries related" do
      test "Inverse posixTimeToSlot >>> slotToPosixTime " testPosixTimeToSlot
      test "Inverse slotToPosixTime >>> posixTimeToSlot " testSlotToPosixTime
      test "PosixTimeToSlot errors" testPosixTimeToSlotError
    group "Properties" do
      test "UpperRay" $ liftToTest testUpperRay
      test "LowerRay" $ liftToTest testLowerRay
      test "Always" $ liftToTest testAlways
      test "Empty" $ liftToTest testEmpty
      test "FiniteInterval" $ liftToTest testFiniteInterval
      test "Contains" $ liftToTest testContains
      test "Hull" $ liftToTest testHull
      test "Intersection" $ liftToTest testIntersection

loadOgmiosFixture
  :: forall (a :: Type). DecodeAeson a => String -> String -> Effect a
loadOgmiosFixture query hash = do
  contents <- readTextFile UTF8 path
  liftEither $ lmap
    (error <<< ((path <> "\n  ") <> _) <<< printJsonDecodeError)
    (decodeJsonString contents)
  where
  path :: String
  path = Path.concat
    [ "fixtures", "test", "ogmios", query <> "-" <> hash <> ".json" ]

-- To update the eraSummaries and systemStart fixtures, run
-- `spago run --main Test.Ogmios.GenerateFixtures`
-- and take the hashes from the result and insert them here. Make sure the
-- newly generated fixtures are stored in source control, i.e. git.

eraSummariesFixture :: Effect EraSummaries
eraSummariesFixture = do
  { result } :: { result :: OgmiosEraSummaries } <- loadOgmiosFixture
    "queryLedgerState-eraSummaries"
    "d8b19110b9580cddfa3895eea34c2139"
  pure $ unwrap result

systemStartFixture :: Effect SystemStart
systemStartFixture = do
  { result } :: { result :: OgmiosSystemStart } <- loadOgmiosFixture
    "queryNetwork-startTime"
    "02fa6f9e7ed04ebfe3294c7648be54d5"
  pure $ unwrap result

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
      [ "1678100000000"
      , "1698191999000"
      ]
  traverse_ (idTest eraSummaries sysStart identity) posixTimes
  -- With Milliseconds, we generally round down, provided the aren't at the
  -- end  with non-zero excess:
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1666656000000")
    (mkPosixTime "1666656000999")
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1666656000000")
    (mkPosixTime "1666656000500")
  idTest eraSummaries sysStart
    (const $ mkPosixTime "1666656000000")
    (mkPosixTime "1666656000499")
  where
  idTest
    :: EraSummaries
    -> SystemStart
    -> (POSIXTime -> POSIXTime)
    -> POSIXTime
    -> Effect Unit
  idTest es ss transf posixTime = do
    case posixTimeToSlot es ss posixTime of
      Left err -> throwError $ error $ show err
      Right slot -> do
        either (throwError <<< error <<< show) (shouldEqual $ transf posixTime)
          $ slotToPosixTime es ss slot

testSlotToPosixTime :: EraSummaries -> SystemStart -> Effect Unit
testSlotToPosixTime eraSummaries sysStart = do
  -- See *Testing far into the future note during hardforks:* for details on
  -- how far into the future we test with slots when a hardfork occurs.
  let
    slots = mkSlot <$>
      [ 31535999
      , 31535000
      , 957323
      , 259200
      , 258200
      , 34952
      , 7532
      , 232
      , 1
      ]
  traverse_ (idTest eraSummaries sysStart) slots
  where
  idTest :: EraSummaries -> SystemStart -> Slot -> Effect Unit
  idTest es ss slot = do
    case slotToPosixTime es ss slot of
      Left err -> throwError $ error $ show err
      Right posixTime -> do
        either (throwError <<< error <<< show) (shouldEqual slot)
          $ posixTimeToSlot es ss posixTime

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
    :: EraSummaries
    -> SystemStart
    -> POSIXTime
    -> PosixTimeToSlotError
    -> Effect Unit
  errTest es ss posixTime expectedErr = do
    case posixTimeToSlot es ss posixTime of
      Left err -> err `shouldEqual` expectedErr
      Right _ ->
        throwError $ error $ "Test should have failed giving: " <> show
          expectedErr

-- All this test can be generalized to use :
-- forall (a::Type) . Arbitrary a => Ord a => Ring a

testUpperRay :: Effect Unit
testUpperRay = quickCheck test
  where
  test :: Int -> Result
  test value =
    let
      ray = from value
      isIn = value `member` ray <?> "value is member of ray"
      notIn = not ((sub value one) `member` ray)
        <?> "check that " <> show (sub value one) <> " is not in ray"
    in
      withMsg
        ("value : " <> show value <> ", ray :" <> show ray)
        $ isIn &=& notIn

testLowerRay :: Effect Unit
testLowerRay = quickCheck test
  where
  test :: Int -> Result
  test value =
    let
      ray = to value
      isIn = value `member` ray <?> "value is member of ray"
      notIn = not ((add one value) `member` ray)
        <?> "check that " <> show (add one value) <> " is not in ray"
    in
      withMsg
        ("value : " <> show value <> ", ray :" <> show ray)
        $ isIn &=& notIn

testAlways :: Effect Unit
testAlways = quickCheck test
  where
  test :: Int -> Result
  test value = value `member` always
    <?> "check that " <> show value <> ", is in always"

testEmpty :: Effect Unit
testEmpty = quickCheck test
  where
  test :: Int -> Result
  test value = (not $ value `member` never)
    <?> "check that " <> show value <> ", isn't in empty"

testFiniteInterval :: Effect Unit
testFiniteInterval = quickCheck test
  where
  test :: Int -> Int -> Result
  test in1 in2 =
    let
      start = BigInt.fromInt (min in1 in2)
      end = BigInt.fromInt (max in1 in2)
      inter = mkFiniteInterval start end
      startIn = start `member` inter <?> "start in interval"
      endIn = end `member` inter <?> "end in interval"
      beforeNotIn = (not $ (sub start one) `member` inter)
        <?> "values before start aren't in interval"
      afterNotIn = (not $ (add one end) `member` inter)
        <?> "values after end aren't in interval"
    in
      withMsg
        ( "start : " <> show start
            <> ", end : "
            <> show end
            <> ", interval :"
            <> show inter
        )
        $ startIn &=& endIn &=& beforeNotIn &=& afterNotIn

testContains :: Effect Unit
testContains = quickCheck test
  where
  test :: Int -> Interval Int -> Result
  test value interval =
    let
      inAlways =
        value `member` always
          && always `contains` interval
          <?> "always has all the values and intervals"
      cantContainAlways =
        ( interval == always
            || not (interval `contains` always)
        )
          <?> "always can't be contained in other intervals"
      hasEmpty = interval `contains` never
        <?> "i `contains` EmptyInterval"
      notInEmpty =
        not (value `member` never)
          &&
            ( never == interval
                || not (never `contains` interval)
            )
          <?> "empty can't contain others values or intervals"
      reflexivity = interval `contains` interval
        <?> "i `contains` i"
    in
      withMsg
        ( "value : " <> show value
            <> ", interval : "
            <> show interval
        )
        $ inAlways &=& cantContainAlways &=& hasEmpty &=& notInEmpty
            &=& reflexivity

testHull :: Effect Unit
testHull = quickCheck test
  where
  test :: Interval Int -> Interval Int -> Result
  test i1 i2 =
    let
      theHull = hull i1 i2
      contains1 = theHull `contains` i1 <?> "i1 contained in `hull i1 i2`"
      contains2 = theHull `contains` i2 <?> "i2 contained in `hull i1 i2`"
      symmetric = hull i2 i1 == theHull <?> "`hull i1 i2 == hull i2 i1`"
      -- since it's symmetric we only need to check this for idempotence
      idempotent = hull theHull i2 == theHull
        <?> "`hull (hull i1 i2) i1 == hull i1 i2`"
      alwaysTest =
        hull i1 always `contains` i1
          && hull i2 always `contains` i2
          <?> "hull i always `contains` i"
    in
      withMsg
        ( "i1 : " <> show i1
            <> ", i2 : "
            <> show i2
            <> ", hull i1 i2 : "
            <> show theHull
        )
        $ contains1 &=& contains2 &=& symmetric &=& idempotent &=& alwaysTest

testIntersection :: Effect Unit
testIntersection = quickCheck test
  where
  test :: Interval Int -> Interval Int -> Result
  test i1 i2 =
    let
      theHull = hull i1 i2
      inter1 = intersection i1 theHull
      inHull = inter1 == i1 <?> "i1 `intersection` hull i1 i2 is i1"
      symmetric =
        inter1 == intersection theHull i1
          && intersection i1 i2 == intersection i2 i1
          <?> "intersection i1 i2 = intersection i2 i1"
      idempotent =
        intersection i1 inter1 == inter1
          && intersection i1 (intersection i1 i2) == intersection i1 i2
          <?> "intersection i1 (intersection i1 i2) == intersection i1 i2"
      neverTest =
        isEmpty (intersection i1 never)
          && isEmpty (intersection i2 never)
          <?> "intersection i EmptyInterval == EmptyInterval"
    in
      withMsg
        ( "i1 : " <> show i1
            <> ", i2 : "
            <> show i2
            <> ", hull i1 i2 : "
            <> show theHull
            <> ", inter : "
            <> show inter1
        )
        $ inHull &=& symmetric &=& idempotent &=& neverTest

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

liftToTest :: Effect Unit -> (EraSummaries -> SystemStart -> Effect Unit)
liftToTest = pure <<< pure

withMsg :: String -> Result -> Result
withMsg _ Success = Success
withMsg msg (Failed original) = Failed $ "(" <> msg <> ") : " <> original

mkPosixTime :: String -> POSIXTime
mkPosixTime = POSIXTime <<< unsafePartial fromJust <<< BigInt.fromString
