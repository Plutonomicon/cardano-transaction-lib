-- | Tests for `ToData`/`FromData` for Interval
module Test.Ctl.Data.Interval (suite) where

import Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , genericFromData
  , genericToData
  , toData
  )
import Contract.Prelude (Aff, liftEffect)
import Contract.Time (always, never)
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.TypeLevel.Nat (Z)
import Ctl.Internal.Types.Interval
  ( Extended(NegInf, Finite, PosInf)
  , Interval(EmptyInterval, StartAt, EndAt, AlwaysInterval, FiniteInterval)
  , LowerBound(LowerBound)
  , UpperBound(UpperBound)
  , genFiniteInterval
  , genLowerRay
  , genUpperRay
  , mkFiniteInterval
  )
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote (group, test)
import Test.QuickCheck (Result, arbitrary, quickCheck, (===))
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Interval" do
    group "Convertion between old interval and new" do
      test "Inverse plutusIntervalToInterval <<< intervalToPlutusInterval" $
        testIntervalConvertion
    group "ToData" do
      -- We separate it by cases instead of use the general `arbitrary`
      -- instance for `Interval` since we are writing by hand the
      -- `ToData` instance for every case.
      test "FiniteInterval"
        $ mkToDataTest
        $ genFiniteInterval genNoNegative
      test "StartAt"
        $ mkToDataTest
        $ genLowerRay genNoNegative
      test "EndAt"
        $ mkToDataTest
        $ genUpperRay genNoNegative
      test "Always" $
        ( toData (always :: Interval BigInt) `shouldEqual`
            (toData <<< intervalToPlutusInterval) (always :: Interval BigInt)
        )
      test "EmptyInterval" $
        ( toData (never :: Interval BigInt) `shouldEqual`
            (toData <<< intervalToPlutusInterval) (never :: Interval BigInt)
        )
    group "FromData" do
      test "FiniteInterval"
        $ mkFromDataTest
        $ genFiniteInterval genNoNegative
      test "StartAt"
        $ mkFromDataTest
        $ genLowerRay genNoNegative
      test "EndAt"
        $ mkFromDataTest
        $ genUpperRay genNoNegative
      test "Always" $
        ( pure (always :: Interval BigInt) `shouldEqual`
            (fromData <<< toData <<< intervalToPlutusInterval)
              (always :: Interval BigInt)
        )
      test "EmptyInterval" $
        ( pure (never :: Interval BigInt) `shouldEqual`
            (fromData <<< toData <<< intervalToPlutusInterval)
              (never :: Interval BigInt)
        )

mkToDataTest :: Gen (Interval Int) -> Aff Unit
mkToDataTest generator = liftEffect $ quickCheck test
  where
  test :: Gen Result
  test = do
    (interval :: Interval Int) <- generator
    let interval' = BigInt.fromInt <$> interval
    pure
      (toData interval' === (toData <<< intervalToPlutusInterval) interval')

mkFromDataTest :: Gen (Interval Int) -> Aff Unit
mkFromDataTest generator = liftEffect $ quickCheck test
  where
  test = do
    (interval :: Interval Int) <- generator
    let interval' = BigInt.fromInt <$> interval
    pure
      ( pure interval' ===
          (fromData <<< toData <<< intervalToPlutusInterval) interval'
      )

testIntervalConvertion :: Aff Unit
testIntervalConvertion = liftEffect $ quickCheck test
  where
  test :: Int -> Int -> Result
  test in1 in2 =
    let
      start' = min in1 in2
      end' = max in1 in2
      inter = mkFiniteInterval start' end'
    in
      inter ===
        (plutusIntervalToInterval <<< intervalToPlutusInterval) inter

-- | An interval of `a`s in the same sense as in plutus.
-- |
-- | The interval may be either closed or open at either end, meaning
-- | that the endpoints may or may not be included in the interval.
-- |
-- | The interval can also be unbounded on either side.
newtype PlutusInterval :: Type -> Type
newtype PlutusInterval a = PlutusInterval
  { from :: LowerBound a, to :: UpperBound a }

derive instance Generic (PlutusInterval a) _
derive newtype instance Eq a => Eq (PlutusInterval a)
derive instance Functor PlutusInterval
derive instance Newtype (PlutusInterval a) _

instance
  HasPlutusSchema (PlutusInterval a)
    ( "PlutusInterval"
        :=
          ( "from" := I (LowerBound a)
              :+ "to"
              := I (UpperBound a)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance Show a => Show (PlutusInterval a) where
  show = genericShow

instance ToData a => ToData (PlutusInterval a) where
  toData = genericToData

instance FromData a => FromData (PlutusInterval a) where
  fromData i = genericFromData i

subtractOne :: forall (a :: Type). Ring a => a -> a
subtractOne x = x `add` (negate one)

addOne :: forall (a :: Type). Semiring a => a -> a
addOne x = x + one

absoluteValue :: Int -> Int
absoluteValue x = if x < 0 then -x else x

genNoNegative :: Gen Int
genNoNegative = absoluteValue <$> arbitrary

intervalToPlutusInterval
  :: forall (a :: Type)
   . Semiring a
  => Ord a
  => Interval a
  -> PlutusInterval a
intervalToPlutusInterval (FiniteInterval start end) = PlutusInterval
  { from: LowerBound (Finite start) true
  , to: UpperBound (Finite $ addOne end) false
  }
intervalToPlutusInterval (StartAt end) = PlutusInterval
  { from: LowerBound NegInf true
  , to: UpperBound (Finite $ addOne end) false
  }
intervalToPlutusInterval (EndAt start) = PlutusInterval
  { from: LowerBound (Finite $ start) true
  , to: UpperBound PosInf true
  }
intervalToPlutusInterval AlwaysInterval = PlutusInterval
  { from: LowerBound NegInf true
  , to: UpperBound PosInf true
  }
intervalToPlutusInterval EmptyInterval = PlutusInterval
  { from: LowerBound PosInf true
  , to: UpperBound NegInf true
  }

plutusIntervalToInterval
  :: forall (a :: Type)
   . Ring a
  => Ord a
  => PlutusInterval a
  -> Interval a
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) true
      , to: UpperBound (Finite end) false
      }
  ) =
  mkFiniteInterval start (subtractOne end)
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound NegInf _
      , to: UpperBound (Finite end) false
      }
  ) =
  StartAt $ subtractOne end
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) true
      , to: UpperBound PosInf _
      }
  ) =
  EndAt start
plutusIntervalToInterval
  (PlutusInterval { from: LowerBound NegInf _, to: UpperBound PosInf _ }) =
  AlwaysInterval
plutusIntervalToInterval
  (PlutusInterval { from: LowerBound PosInf _, to: UpperBound NegInf _ }) =
  EmptyInterval
-- All of the remain cases are to avoid using a `Maybe` or throwing exception.
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) true
      , to: UpperBound (Finite end) true
      }
  ) =
  mkFiniteInterval start end
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) false
      , to: UpperBound (Finite end) true
      }
  ) =
  mkFiniteInterval (addOne start) end
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) false
      , to: UpperBound (Finite end) false
      }
  ) =
  mkFiniteInterval (addOne start) (subtractOne end)
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound NegInf _
      , to: UpperBound (Finite end) true
      }
  ) =
  StartAt end
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound (Finite start) false
      , to: UpperBound PosInf _
      }
  ) =
  EndAt $ addOne start
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound _ _
      , to: UpperBound NegInf _
      }
  ) = EmptyInterval
plutusIntervalToInterval
  ( PlutusInterval
      { from: LowerBound PosInf _
      , to: UpperBound _ _
      }
  ) = EmptyInterval
