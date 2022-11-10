module Test.Ctl.MustSpendTotal
  ( main
  , suite
  ) where

import Prelude

import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy)
import Contract.TxConstraints
  ( TxConstraints
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  )
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicyMaybe)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Exception (error)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

-- Run with `spago test --main Test.Ctl.MustSpendTotal`
main :: Effect Unit
main = launchAff_ do
  interpret suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "mustXAtLeastTotal works correctly" do
    test "mustSpendAtLeastTotal works correctly" $ do
      testIsAuthomorhism mustSpendAtLeast mustSpendAtLeastTotal
    test "mustProduceAtLeastTotal works correctly" $ do
      testIsAuthomorhism mustProduceAtLeast mustProduceAtLeastTotal

testIsAuthomorhism
  :: forall i o
   . (Value.Value -> TxConstraints i o)
  -> (TxConstraints i o -> Value.Value)
  -> Aff Unit
testIsAuthomorhism valueToConstraint constraintToValue = do
  token1 <- mkTokenName "Token 1"
  token2 <- mkTokenName "Token 2"
  cs <- mkCurrencySymbol alwaysMintsPolicyMaybe
  let
    value1 = Value.singleton cs token1 one
    value2 = Value.singleton cs token2 one
    constraint = valueToConstraint value1 <> valueToConstraint value2
  (constraintToValue constraint) `shouldEqual` (value1 <> value2)

-- Helpers

mkTokenName :: String -> Aff TokenName
mkTokenName str =
  liftMaybe (error "Cannot decode token name") $
    (Value.mkTokenName <=< byteArrayFromAscii) str

mkCurrencySymbol :: Maybe MintingPolicy -> Aff CurrencySymbol
mkCurrencySymbol policy = liftMaybe (error "Cannot decode policy") $
  (policy >>= Value.scriptCurrencySymbol)
