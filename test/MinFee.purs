module Test.MinFee where

import Prelude

import Cardano.Types.Transaction (Transaction)
import Data.Either (hush)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Helpers (liftedM)
import Mote (group, test)
import QueryM (calculateMinFeeOld, runQueryM)
import QueryM.Config (testnetTraceQueryConfig)
import QueryM.MinFee (calculateMinFee)
import Test.Fixtures (txFixture1, txFixture2, txFixture3, txFixture4)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  -- Test UtxosAt using internal types.
  group "min_fee: CSL vs. server" do
    test "Tx fixture #1" do
      testTxFixture txFixture1
    test "Tx fixture #2" do
      testTxFixture txFixture2
    test "Tx fixture #3" do
      testTxFixture txFixture3
    test "Tx fixture #4" do
      testTxFixture txFixture4

testTxFixture :: Transaction -> Aff Unit
testTxFixture tx = do
  cslFee /\ serverFee <- runQueryM testnetTraceQueryConfig do
    cslFee <- calculateMinFee txFixture1
    serverFee <- liftedM (error ":(") $ hush <$> calculateMinFeeOld tx
    pure $ cslFee /\ serverFee
  cslFee `shouldEqual` serverFee
  pure unit
