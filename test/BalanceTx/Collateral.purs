module Test.BalanceTx.Collateral (suite) where

import Prelude

import BalanceTx.Collateral.Select
  ( maxCandidateUtxos
  , minRequiredCollateral
  , selectCollateral
  )
import BalanceTx.FakeOutput (fakeOutputWithValue)
import Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Coin(Coin), Value(Value))
import Cardano.Types.Value (lovelaceValueOf, mkSingletonNonAdaAsset) as Value
import Control.Monad.Reader.Trans (asks)
import Ctl.Internal.Test.Utils (Seconds(Seconds), measure, measureWithTimeout)
import Data.Array (length, range, replicate, zipWith) as Array
import Data.BigInt (fromInt) as BigInt
import Data.List (singleton) as List
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt (fromInt, toInt) as UInt
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Mote (group, test)
import QueryM (QueryM, runQueryM)
import QueryM.Config (testnetTraceQueryConfig)
import QueryM.Ogmios (CoinsPerUtxoUnit)
import Test.Fixtures (currencySymbol1, tokenName1, tokenName2, txInputFixture1)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.Transaction (TransactionHash, TransactionInput)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "BalanceTx.Collateral" do
    group "selectCollateral" do
      test "Prefers a single Ada-only inp if it covers minRequiredCollateral" do
        withParams \coinsPerUtxoUnit maxCollateralInputs -> do
          collateral <-
            measure
              $ liftEffect
              $ selectCollateral coinsPerUtxoUnit maxCollateralInputs
                  utxosFixture1
          collateral `shouldEqual`
            Just (List.singleton $ txUnspentOut zero adaOnlyTxOutputSuf)

      test "Prefers an input with the lowest min ada for collateral output" do
        withParams \coinsPerUtxoUnit maxCollateralInputs -> do
          collateral <-
            measure
              $ liftEffect
              $ selectCollateral coinsPerUtxoUnit maxCollateralInputs
                  utxosFixture2
          collateral `shouldEqual`
            Just (List.singleton $ txUnspentOut zero singleAssetTxOutputSuf)

      test "Selects a collateral in less than 2 seconds" do
        withParams \coinsPerUtxoUnit maxCollateralInputs ->
          measureWithTimeout (Seconds 2.0)
            ( void $ liftEffect $ selectCollateral coinsPerUtxoUnit
                maxCollateralInputs
                utxosFixture3
            )

withParams :: (CoinsPerUtxoUnit -> Int -> QueryM Unit) -> Aff Unit
withParams test =
  runQueryM testnetTraceQueryConfig
    (join (test <$> getCoinsPerUtxoUnit <*> getMaxCollateralInputs))
  where
  getMaxCollateralInputs :: QueryM Int
  getMaxCollateralInputs =
    asks $ _.runtime >>> _.pparams <#>
      UInt.toInt <<< _.maxCollateralInputs <<< unwrap

  getCoinsPerUtxoUnit :: QueryM CoinsPerUtxoUnit
  getCoinsPerUtxoUnit =
    asks (_.runtime >>> _.pparams) <#> unwrap >>>
      _.coinsPerUtxoUnit

-- | Ada-only tx output sufficient to cover `minRequiredCollateral`.
adaOnlyTxOutputSuf :: TransactionOutput
adaOnlyTxOutputSuf =
  fakeOutputWithValue $
    Value.lovelaceValueOf (minRequiredCollateral + one)

-- | Ada-only tx output insufficient to cover `minRequiredCollateral`.
adaOnlyTxOutputInsuf :: TransactionOutput
adaOnlyTxOutputInsuf =
  fakeOutputWithValue $
    Value.lovelaceValueOf (minRequiredCollateral / BigInt.fromInt 2)

-- | Single-asset tx output sufficient to cover `minRequiredCollateral`.
singleAssetTxOutputSuf :: TransactionOutput
singleAssetTxOutputSuf =
  fakeOutputWithValue
    $ Value (Coin $ minRequiredCollateral + one)
    $ Value.mkSingletonNonAdaAsset currencySymbol1 tokenName1 one

-- | Multi-asset tx output sufficient to cover `minRequiredCollateral`.
multiAssetTxOutputSuf :: TransactionOutput
multiAssetTxOutputSuf =
  fakeOutputWithValue
    $ Value (Coin $ minRequiredCollateral + one)
    $ Value.mkSingletonNonAdaAsset currencySymbol1 tokenName1 one
        <> Value.mkSingletonNonAdaAsset currencySymbol1 tokenName2 one

utxosFixture1 :: UtxoMap
utxosFixture1 =
  mkUtxosFixture
    [ adaOnlyTxOutputSuf, adaOnlyTxOutputInsuf, singleAssetTxOutputSuf ]

utxosFixture2 :: UtxoMap
utxosFixture2 =
  mkUtxosFixture
    [ singleAssetTxOutputSuf, multiAssetTxOutputSuf ]

utxosFixture3 :: UtxoMap
utxosFixture3 =
  mkUtxosFixture $
    Array.replicate (maxCandidateUtxos * 100) adaOnlyTxOutputInsuf

mkUtxosFixture :: Array TransactionOutput -> UtxoMap
mkUtxosFixture txOuts =
  Map.fromFoldable $
    Array.zipWith utxo (Array.range 0 $ Array.length txOuts - 1) txOuts

-- | Constructs an utxo with given tx output and its index.
utxo :: Int -> TransactionOutput -> TransactionInput /\ TransactionOutput
utxo index = utxo' (UInt.fromInt index)

utxo' :: UInt -> TransactionOutput -> TransactionInput /\ TransactionOutput
utxo' index = Tuple (wrap { transactionId, index })

-- | Constructs a `TransactionUnspentOutput` with given tx output and its index.
txUnspentOut :: Int -> TransactionOutput -> TransactionUnspentOutput
txUnspentOut index =
  utxo index >>> \(input /\ output) -> wrap { input, output }

transactionId :: TransactionHash
transactionId = _.transactionId (unwrap txInputFixture1)
