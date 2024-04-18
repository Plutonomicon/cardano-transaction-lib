module Test.Ctl.BalanceTx.Collateral (suite) where

import Prelude

import Cardano.Types
  ( Coin(Coin)
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , TransactionUnspentOutput
  , UtxoMap
  , Value(Value)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.Value as Value
import Contract.Config (testnetConfig)
import Contract.Monad (Contract, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Ctl.Internal.BalanceTx.Collateral.Select
  ( maxCandidateUtxos
  , minRequiredCollateral
  , selectCollateral
  )
import Ctl.Internal.BalanceTx.FakeOutput (fakeOutputWithValue)
import Data.Array (length, range, replicate, zipWith) as Array
import Data.List (singleton) as List
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt (fromInt, toInt) as UInt
import Effect.Aff (Aff)
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures
  ( currencySymbol1
  , tokenName1
  , tokenName2
  , txInputFixture1
  )
import Test.Ctl.Utils (measure, measureWithTimeout)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "BalanceTx.Collateral" do
    group "selectCollateral" do
      test "Prefers a single Ada-only inp if it covers minRequiredCollateral" do
        withParams \coinsPerUtxoUnit maxCollateralInputs -> do
          collateral <-
            measure
              $ pure
              $ selectCollateral coinsPerUtxoUnit maxCollateralInputs
                  utxosFixture1
          collateral `shouldEqual`
            Just (List.singleton $ txUnspentOut zero adaOnlyTxOutputSuf)

      test "Prefers an input with the lowest min ada for collateral output" do
        withParams \coinsPerUtxoUnit maxCollateralInputs -> do
          collateral <-
            measure
              $ pure
              $ selectCollateral coinsPerUtxoUnit maxCollateralInputs
                  utxosFixture2
          collateral `shouldEqual`
            Just (List.singleton $ txUnspentOut zero singleAssetTxOutputSuf)

      test "Selects a collateral in less than 2 seconds" do
        withParams \coinsPerUtxoUnit maxCollateralInputs ->
          measureWithTimeout (Seconds 2.0)
            ( void $ pure $ selectCollateral coinsPerUtxoUnit
                maxCollateralInputs
                utxosFixture3
            )

withParams :: (Coin -> Int -> Contract Unit) -> Aff Unit
withParams test =
  runContract testnetConfig { suppressLogs = true }
    (join (test <$> getCoinsPerUtxoUnit <*> getMaxCollateralInputs))
  where
  getMaxCollateralInputs :: Contract Int
  getMaxCollateralInputs =
    getProtocolParameters <#>
      UInt.toInt <<< _.maxCollateralInputs <<< unwrap

  getCoinsPerUtxoUnit :: Contract Coin
  getCoinsPerUtxoUnit =
    getProtocolParameters <#> unwrap >>>
      _.coinsPerUtxoByte

-- | Ada-only tx output sufficient to cover `minRequiredCollateral`.
adaOnlyTxOutputSuf :: TransactionOutput
adaOnlyTxOutputSuf =
  fakeOutputWithValue $
    Value.lovelaceValueOf
      ( unsafePartial $ fromJust $ BigNum.add BigNum.one $ unwrap
          minRequiredCollateral
      )

-- | Ada-only tx output insufficient to cover `minRequiredCollateral`.
adaOnlyTxOutputInsuf :: TransactionOutput
adaOnlyTxOutputInsuf =
  fakeOutputWithValue
    $ Value.lovelaceValueOf
    $ unsafePartial
    $ fromJust
    $ BigNum.fromBigInt
    $ BigNum.toBigInt (unwrap minRequiredCollateral) / BigInt.fromInt 2

-- | Single-asset tx output sufficient to cover `minRequiredCollateral`.
singleAssetTxOutputSuf :: TransactionOutput
singleAssetTxOutputSuf =
  fakeOutputWithValue
    $ Value
        ( Coin $ unsafePartial $ fromJust $ BigNum.add BigNum.one $ unwrap
            minRequiredCollateral
        )
    $ MultiAsset.singleton currencySymbol1 tokenName1 BigNum.one

-- | Multi-asset tx output sufficient to cover `minRequiredCollateral`.
multiAssetTxOutputSuf :: TransactionOutput
multiAssetTxOutputSuf = unsafePartial
  $ fakeOutputWithValue
  $ Value
      ( Coin $ unsafePartial $ fromJust $ BigNum.add BigNum.one $ unwrap
          minRequiredCollateral
      )
  $ MultiAsset.singleton currencySymbol1 tokenName1 BigNum.one
      <> MultiAsset.singleton currencySymbol1 tokenName2 BigNum.one

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
