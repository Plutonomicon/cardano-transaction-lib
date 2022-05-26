module Test.Plutus.Value (suite) where

import Prelude

import Data.Array ((..), length, zip)
import Data.BigInt (fromInt)
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (for_)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Plutus.Conversion (fromPlutusValue, toPlutusValue)
import Plutus.Types.CurrencySymbol (CurrencySymbol) as Plutus
import Plutus.Types.CurrencySymbol as Plutus.CurrencySymbol
import Plutus.Types.Value (Value) as Plutus
import Plutus.Types.Value as Plutus.Value
import Test.Fixtures (currencySymbol1, tokenName1, tokenName2)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Cardano.Types.Value (Value) as Types
import Cardano.Types.Value as Value

suite :: TestPlanM Unit
suite = do
  group "Plutus.Types.Value" $ do
    group "FromPlutusType & ToPlutusType" $ do
      let indices = 0 .. (length testData - 1)
      for_ (zip testData indices) $ \((valuePlutus /\ value) /\ i) ->
        toFromPlutusTypeTest i valuePlutus value

toFromPlutusTypeTest
  :: Int -> Plutus.Value -> Types.Value -> TestPlanM Unit
toFromPlutusTypeTest i valuePlutus value =
  test (show i <> ": Plutus.Types.Value <-> Types.Value") $ do
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
