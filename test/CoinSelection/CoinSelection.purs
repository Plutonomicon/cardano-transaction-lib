module Test.Ctl.CoinSelection (suite) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionState
  , SelectionStrategy(SelectionStrategyMinimal, SelectionStrategyOptimal)
  , performMultiAssetSelection
  )
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  )
import Ctl.Internal.Cardano.Types.Value
  ( CurrencySymbol
  , NonAdaAsset
  , Value
  , mkCoin
  , mkCurrencySymbol
  , mkSingletonNonAdaAsset
  , mkValue
  )
import Ctl.Internal.CoinSelection.UtxoIndex (UtxoIndex)
import Ctl.Internal.CoinSelection.UtxoIndex (buildUtxoIndex) as UtxoIndex
import Ctl.Internal.Hashing (blake2b224Hash)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii)
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Ctl.Internal.Types.TokenName (TokenName, mkTokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (fromInt) as BigInt
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (empty, fromFoldable, values) as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.CoinSelection.Arbitrary
  ( ArbitraryAddress
  , ArbitraryTransactionInput
  , ArbitraryUtxoIndex
  )
import Test.Ctl.CoinSelection.UtxoIndex (suite) as UtxoIndex
import Test.QuickCheck (class Testable, Result, assertEquals)
import Test.QuickCheck (test) as QuickCheck
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSampleOne)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "CoinSelection" do
    UtxoIndex.suite
    group "performMultiAssetSelection" do
      performMultiAssetSelection_unitTests
      test "Performs a selection with zero outputs" do
        quickCheck prop_performMultiAssetSelection_empty

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

prop_performMultiAssetSelection_empty
  :: SelectionStrategy -> ArbitraryUtxoIndex -> CoinSelectionTestM Result
prop_performMultiAssetSelection_empty strategy utxoIndex =
  assertEquals targetSelState <$>
    performMultiAssetSelection strategy (unwrap utxoIndex) mempty
  where
  targetSelState :: SelectionState
  targetSelState =
    wrap { leftoverUtxos: unwrap utxoIndex, selectedUtxos: Map.empty }

performMultiAssetSelection_unitTests :: TestPlanM (Aff Unit) Unit
performMultiAssetSelection_unitTests =
  for_ selFixtures \{ testLabel, strategy, inpFixture, outFixture } -> do
    let testLabel' = testLabel <> showSelStrategy strategy
    test testLabel' $ liftEffect $ (unwrap :: CoinSelectionTestM _ -> _) do
      td <- liftEffect $
        selTestDataFromFixtures (inpFixture strategy) (outFixture strategy)
      selectedUtxos <-
        _.selectedUtxos <<< unwrap <$>
          performMultiAssetSelection td.strategy td.utxoIndex td.requiredValue
      liftEffect $
        td.selectedValue `shouldEqual`
          foldMap (_.amount <<< unwrap) (Map.values selectedUtxos)

showSelStrategy :: SelectionStrategy -> String
showSelStrategy strategy =
  case strategy of
    SelectionStrategyOptimal -> " (optimal strategy)"
    SelectionStrategyMinimal -> " (minimal strategy)"

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

data AssetFixture = AssetA | AssetB | AssetC

derive instance Generic AssetFixture _

instance Show AssetFixture where
  show = genericShow

assetClassFromFixture :: AssetFixture -> CurrencySymbol /\ TokenName
assetClassFromFixture asset =
  currencySymbolFromAscii (show asset) /\ tokenNameFromAscii (show asset)
  where
  currencySymbolFromAscii :: String -> CurrencySymbol
  currencySymbolFromAscii str =
    unsafePartial fromJust $
      mkCurrencySymbol =<< map blake2b224Hash (byteArrayFromAscii str)

  tokenNameFromAscii :: String -> TokenName
  tokenNameFromAscii =
    unsafePartial fromJust <<< (mkTokenName <=< byteArrayFromAscii)

type TokenBundleFixture = Int /\ Array (AssetFixture /\ Int)

assetFromFixture :: AssetFixture /\ Int -> NonAdaAsset
assetFromFixture (assetFixture /\ quantity) =
  mkSingletonNonAdaAsset currencySymbol tokenName (BigInt.fromInt quantity)
  where
  currencySymbol /\ tokenName = assetClassFromFixture assetFixture

valueFromFixture :: TokenBundleFixture -> Value
valueFromFixture (coin /\ assets) =
  mkValue (mkCoin coin) (foldMap assetFromFixture assets)

type SelInputFixture =
  { strategy :: SelectionStrategy
  , requiredValue :: TokenBundleFixture
  , utxos :: Array TokenBundleFixture
  }

type SelOutputFixture = Array TokenBundleFixture

type SelTestData =
  { strategy :: SelectionStrategy
  , requiredValue :: Value
  , utxoIndex :: UtxoIndex
  , selectedValue :: Value
  }

selTestDataFromFixtures
  :: SelInputFixture -> SelOutputFixture -> Effect SelTestData
selTestDataFromFixtures inpFixture outFixture = do
  utxoIndex <-
    UtxoIndex.buildUtxoIndex <<< Map.fromFoldable <$>
      for inpFixture.utxos \bundle ->
        Tuple <$> txInputSample <*> mkTxOutput (valueFromFixture bundle)
  pure
    { strategy: inpFixture.strategy
    , requiredValue: valueFromFixture inpFixture.requiredValue
    , utxoIndex
    , selectedValue: fold (valueFromFixture <$> outFixture)
    }
  where
  txInputSample :: Effect TransactionInput
  txInputSample =
    unwrap <$> randomSampleOne (arbitrary :: Gen ArbitraryTransactionInput)

  mkTxOutput :: Value -> Effect TransactionOutput
  mkTxOutput amount = do
    address <- unwrap <$> randomSampleOne (arbitrary :: Gen ArbitraryAddress)
    pure $ TransactionOutput
      { address, amount, datum: NoOutputDatum, scriptRef: Nothing }

--------------------------------------------------------------------------------

selInputFixture0 :: SelectionStrategy -> SelInputFixture
selInputFixture0 strategy =
  { strategy
  , requiredValue: 100 /\ [ AssetA /\ 5 ]
  , utxos:
      [ 40 /\ [ AssetA /\ 5 ] -- singleton for AssetA
      , 40 /\ [ AssetA /\ 5 ] -- singleton for AssetA
      , 60 /\ mempty -- singleton for AssetLovelace
      , 10 /\ [ AssetA /\ 4, AssetB /\ 1 ] -- pair for AssetA and AssetB
      , 100 /\ [ AssetA /\ 5, AssetB /\ 1, AssetC /\ 1 ] -- multiple assets
      ]
  }

selOutputFixture0 :: SelectionStrategy -> SelOutputFixture
selOutputFixture0 strategy =
  case strategy of
    SelectionStrategyOptimal ->
      [ 40 /\ [ AssetA /\ 5 ], 40 /\ [ AssetA /\ 5 ], 60 /\ mempty ]
    SelectionStrategyMinimal ->
      [ 40 /\ [ AssetA /\ 5 ], 60 /\ mempty ]

selInputFixture1 :: SelectionStrategy -> SelInputFixture
selInputFixture1 strategy =
  { strategy
  , requiredValue: 100 /\ [ AssetA /\ 5 ]
  , utxos:
      [ 100 /\ [ AssetA /\ 5 ]
      -- ^ singleton for AssetA - covers the output asset quantity,
      -- but the selection can still be improved
      , 50 /\ [ AssetA /\ 5, AssetB /\ 1 ]
      -- ^ pair for AssetA and AssetB - should not be considered to
      -- improve the selection for AssetA
      , 50 /\ [ AssetA /\ 4, AssetB /\ 1, AssetC /\ 1 ]
      -- ^ bundle containing multiple assets including AssetA - should not be
      -- considered to improve the selection for AssetA
      ]
  }

selOutputFixture1 :: SelectionStrategy -> SelOutputFixture
selOutputFixture1 _ = [ 100 /\ [ AssetA /\ 5 ] ]

--------------------------------------------------------------------------------

type SelectionTest =
  { testLabel :: String
  , strategy :: SelectionStrategy
  , inpFixture :: SelectionStrategy -> SelInputFixture
  , outFixture :: SelectionStrategy -> SelOutputFixture
  }

selTestsForBothStrategies
  :: (SelectionStrategy -> SelInputFixture)
  -> (SelectionStrategy -> SelOutputFixture)
  -> String
  -> Array SelectionTest
selTestsForBothStrategies inpFixture outFixture testLabel =
  [ mkTest SelectionStrategyOptimal, mkTest SelectionStrategyMinimal ]
  where
  mkTest :: SelectionStrategy -> SelectionTest
  mkTest strategy = { testLabel, strategy, inpFixture, outFixture }

selFixtures :: Array SelectionTest
selFixtures =
  selTestsForBothStrategies selInputFixture0 selOutputFixture0
    "Selects only from the 'singletons' subset if possible"
    <>
      selTestsForBothStrategies selInputFixture1 selOutputFixture1
        "Selects only from the 'singletons' subset to improve selection"

--------------------------------------------------------------------------------
-- CoinSelectionTestM
--------------------------------------------------------------------------------

newtype CoinSelectionTestM (a :: Type) = CoinSelectionTestM (Effect a)

derive instance Newtype (CoinSelectionTestM a) _
derive newtype instance Functor CoinSelectionTestM
derive newtype instance Apply CoinSelectionTestM
derive newtype instance Applicative CoinSelectionTestM
derive newtype instance Bind CoinSelectionTestM
derive newtype instance Monad CoinSelectionTestM
derive newtype instance MonadEffect CoinSelectionTestM

instance MonadThrow BalanceTxError CoinSelectionTestM where
  throwError = liftEffect <<< throw <<< show

instance Testable prop => Testable (CoinSelectionTestM prop) where
  test = QuickCheck.test <<< unsafePerformEffect <<< unwrap
