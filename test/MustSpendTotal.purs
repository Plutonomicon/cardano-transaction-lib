module Test.Ctl.MustSpendTotal
  ( main
  , suite
  ) where

import Prelude hiding (join)

import Contract.TxConstraints
  ( TxConstraints
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  )
import Contract.Value as Value
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Lattice (join)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Test.QuickCheck (Result, (<?>), (===))
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
      quickCheck $
        prop_roundtripOverMonoid mustSpendAtLeast mustSpendAtLeastTotal
    test "mustProduceAtLeastTotal is roundtrip to mustProduceAtLeast" $ do
      quickCheck $ prop_roundtrip mustProduceAtLeast mustProduceAtLeastTotal
    test "mustProduceAtLeastTotal is homomorphism" do
      quickCheck $
        prop_roundtripOverMonoid mustProduceAtLeast mustProduceAtLeastTotal

-- Properties

prop_roundtripOverMonoid
  :: (Value.Value -> TxConstraints)
  -> (TxConstraints -> Value.Value)
  -> Value.Value
  -> Value.Value
  -> Result
prop_roundtripOverMonoid must total x y =
  (x `join` y) == total (must x <> must y) <?>
    "On value 1 " <> show x <> " and value 2 " <> show y

prop_roundtrip
  :: (Value.Value -> TxConstraints)
  -> (TxConstraints -> Value.Value)
  -> Value.Value
  -> Result
prop_roundtrip must total x = (must >>> total) x === x
