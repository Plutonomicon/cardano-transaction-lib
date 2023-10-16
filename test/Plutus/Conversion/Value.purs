module Test.Ctl.Internal.Plutus.Conversion.Value (suite) where

import Prelude

import Ctl.Internal.Cardano.Types.Value (Value) as Types
import Ctl.Internal.Cardano.Types.Value as Value
import Ctl.Internal.Plutus.Conversion (fromPlutusValue, toPlutusValue)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol) as Plutus
import Ctl.Internal.Plutus.Types.CurrencySymbol as Plutus.CurrencySymbol
import Ctl.Internal.Plutus.Types.Value (Value) as Plutus
import Ctl.Internal.Plutus.Types.Value as Plutus.Value
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Array (length, range, zip)
import Data.Maybe (fromJust)
import Data.Traversable (for_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import JS.BigInt (fromInt)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures (currencySymbol1, tokenName1, tokenName2)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Conversion: Plutus Value <-> Types.Value" $ do
    let indices = 0 `range` (length testData - 1)
    for_ (zip testData indices) $ \((valuePlutus /\ value) /\ i) ->
      toFromPlutusValueTest i valuePlutus value

toFromPlutusValueTest
  :: Int -> Plutus.Value -> Types.Value -> TestPlanM (Aff Unit) Unit
toFromPlutusValueTest i valuePlutus value = do
  test (show i <> ": Performs conversion between `Value`s") $ do
    let resValue = fromPlutusValue valuePlutus
    resValue `shouldEqual` value
    let resValuePlutus = toPlutusValue resValue
    resValuePlutus `shouldEqual` valuePlutus

testData :: Array (Plutus.Value /\ Types.Value)
testData = [ emptyValues, adaValues, nonAdaValues, compositeValues ]
  where
  amount1 /\ amount2 = fromInt 5000 /\ fromInt 6000

  currencySymbol1' :: Plutus.CurrencySymbol
  currencySymbol1' = unsafePartial $ fromJust $
    Plutus.CurrencySymbol.mkCurrencySymbol
      (Value.getCurrencySymbol currencySymbol1)

  nonAdaAsset1 :: Value.NonAdaAsset
  nonAdaAsset1 =
    Value.mkSingletonNonAdaAsset currencySymbol1 tokenName1 amount1

  nonAdaAsset2 :: Value.NonAdaAsset
  nonAdaAsset2 =
    Value.mkSingletonNonAdaAsset currencySymbol1 tokenName2 amount2

  emptyValues :: Plutus.Value /\ Types.Value
  emptyValues = mempty /\ mempty

  adaValues :: Plutus.Value /\ Types.Value
  adaValues =
    Plutus.Value.lovelaceValueOf amount1 /\
      Value.lovelaceValueOf amount1

  nonAdaValues :: Plutus.Value /\ Types.Value
  nonAdaValues =
    Plutus.Value.singleton currencySymbol1' tokenName1 amount1 /\
      Value.mkValue mempty nonAdaAsset1

  compositeValues :: Plutus.Value /\ Types.Value
  compositeValues =
    ( fst adaValues <> fst nonAdaValues <>
        Plutus.Value.singleton currencySymbol1' tokenName2 amount2
    ) /\
      ( snd adaValues <> snd nonAdaValues <>
          Value.mkValue mempty nonAdaAsset2
      )
