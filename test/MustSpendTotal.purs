module Test.Ctl.MustSpendTotal
  ( main
  , suite
  ) where

import Prelude

import Contract.TxConstraints
  ( TxConstraints
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  )
import Contract.Value as Value
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Test.QuickCheck (Result, (===))
import Test.Spec.QuickCheck (quickCheck)

-- Run with `spago test --main Test.Ctl.MustSpendTotal`
main :: Effect Unit
main = launchAff_ do
  interpret suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "mustXAtLeastTotal works correctly" do
    test "mustSpendAtLeastTotal is roundtrip to mustSpendAtLeast" $ do
      quickCheck $ prop_roundtrip mustSpendAtLeast mustSpendAtLeastTotal
    test "mustSpendAtLeastTotal is homomorphism" do
      quickCheck $ prop_isHomomorphism mustSpendAtLeast
    test "mustProduceAtLeastTotal is roundtrip to mustProduceAtLeast" $ do
      quickCheck $ prop_roundtrip mustProduceAtLeast mustProduceAtLeastTotal
    test "mustProduceAtLeastTotal is homomorphism" do
      quickCheck $ prop_isHomomorphism mustProduceAtLeast

-- Properties

prop_isHomomorphism
  :: forall i o
   . (Value.Value -> TxConstraints i o)
  -> Value.Value
  -> Value.Value
  -> Boolean
prop_isHomomorphism f x y = ((f x) <> (f y)) `customEq` (f (x <> y))
  where
  customEq :: TxConstraints i o -> TxConstraints i o -> Boolean
  customEq = ((==) `on` InspectableTxConstraints)

prop_roundtrip
  :: forall i o
   . (Value.Value -> TxConstraints i o)
  -> (TxConstraints i o -> Value.Value)
  -> Value.Value
  -> Result
prop_roundtrip f g x = (f >>> g) x === x

-- Utils

-- | TxConstraint may not has Eq, but our tests require it
-- | This newtype provides less specific Eq which is fine for these tests
newtype InspectableTxConstraints i o = InspectableTxConstraints
  (TxConstraints i o)

derive instance Newtype (InspectableTxConstraints i o) _

instance Eq (InspectableTxConstraints i o) where
  eq x y = (eqOnProjection mustSpendAtLeastTotal)
    && (eqOnProjection mustProduceAtLeastTotal)
    where
    eqOnProjection proj = (((==) `on` (proj <<< unwrap)) x y)
