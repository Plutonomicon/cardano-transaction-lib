module Test.Ctl.CoinSelection (suite) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyMinimal, SelectionStrategyOptimal)
  , mkSelectionState
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
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable, values) as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.CoinSelection.Arbitrary
  ( ArbitraryAddress
  , ArbitraryTransactionInput
  , ArbitraryUtxoIndex
  )
import Test.Ctl.CoinSelection.RoundRobin (suite) as RoundRobin
import Test.Ctl.CoinSelection.SelectionState (suite) as SelectionState
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
    RoundRobin.suite
    SelectionState.suite
    UtxoIndex.suite
    group "performMultiAssetSelection" do
      test "Performs a selection with zero outputs" do
        quickCheck prop_performMultiAssetSelection_empty
      runSelectionTestWithFixture selFixture0
        "Selects only from the 'singletons' subset if possible"
      runSelectionTestWithFixture selFixture1
        "Selects only from the 'singletons' subset to improve selection"
      runSelectionTestWithFixture selFixture2
        "Selects from the 'pairs' subset if the 'singletons' subset is empty"

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

prop_performMultiAssetSelection_empty
  :: SelectionStrategy -> ArbitraryUtxoIndex -> CoinSelectionTestM Result
prop_performMultiAssetSelection_empty strategy utxoIndex =
  assertEquals (mkSelectionState $ unwrap utxoIndex) <$>
    performMultiAssetSelection strategy (unwrap utxoIndex) mempty

runSelectionTestWithFixture
  :: (SelectionStrategy -> SelFixture) -> String -> TestPlanM (Aff Unit) Unit
runSelectionTestWithFixture mkFixture testLabel =
  for_ [ SelectionStrategyOptimal, SelectionStrategyMinimal ] \strategy -> do
    let testLabel' = testLabel <> showSelStrategy strategy
    test testLabel' $ liftEffect $ (unwrap :: CoinSelectionTestM _ -> _) do
      td <- liftEffect $ selTestDataFromFixture (mkFixture strategy)
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

type TokenBundleFixture = Int /\ Array (AssetFixture /\ Int)

type SelFixture =
  { strategy :: SelectionStrategy
  , requiredValue :: TokenBundleFixture
  , utxos :: Array TokenBundleFixture
  , selectedUtxos :: Array TokenBundleFixture
  }

type SelTestData =
  { strategy :: SelectionStrategy
  , requiredValue :: Value
  , utxoIndex :: UtxoIndex
  , selectedValue :: Value
  }

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

assetFromFixture :: AssetFixture /\ Int -> NonAdaAsset
assetFromFixture (assetFixture /\ quantity) =
  mkSingletonNonAdaAsset currencySymbol tokenName (BigInt.fromInt quantity)
  where
  currencySymbol /\ tokenName = assetClassFromFixture assetFixture

valueFromFixture :: TokenBundleFixture -> Value
valueFromFixture (coin /\ assets) =
  mkValue (mkCoin coin) (foldMap assetFromFixture assets)

selTestDataFromFixture :: SelFixture -> Effect SelTestData
selTestDataFromFixture selFixture = do
  utxoIndex <-
    UtxoIndex.buildUtxoIndex <<< Map.fromFoldable <$>
      for selFixture.utxos \bundle ->
        Tuple <$> txInputSample <*> mkTxOutput (valueFromFixture bundle)
  pure
    { strategy: selFixture.strategy
    , requiredValue: valueFromFixture selFixture.requiredValue
    , utxoIndex
    , selectedValue: fold (valueFromFixture <$> selFixture.selectedUtxos)
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

selFixture0 :: SelectionStrategy -> SelFixture
selFixture0 strategy =
  { strategy
  , requiredValue: 100 /\ [ AssetA /\ 5 ]
  , utxos:
      [ 40 /\ [ AssetA /\ 5 ] -- singleton for AssetA
      , 40 /\ [ AssetA /\ 5 ] -- singleton for AssetA
      , 60 /\ mempty -- singleton for AssetLovelace
      , 10 /\ [ AssetA /\ 4, AssetB /\ 1 ] -- pair for AssetA and AssetB
      , 100 /\ [ AssetA /\ 5, AssetB /\ 1, AssetC /\ 1 ] -- multiple assets
      ]
  , selectedUtxos:
      case strategy of
        SelectionStrategyOptimal ->
          [ 40 /\ [ AssetA /\ 5 ], 40 /\ [ AssetA /\ 5 ], 60 /\ mempty ]
        SelectionStrategyMinimal ->
          [ 40 /\ [ AssetA /\ 5 ], 60 /\ mempty ]
  }

selFixture1 :: SelectionStrategy -> SelFixture
selFixture1 strategy =
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
  , selectedUtxos: [ 100 /\ [ AssetA /\ 5 ] ]
  }

selFixture2 :: SelectionStrategy -> SelFixture
selFixture2 strategy =
  { strategy
  , requiredValue: 100 /\ [ AssetA /\ 10 ]
  , utxos:
      [ 50 /\ [ AssetA /\ 4, AssetB /\ 1 ]
      -- ^ pair for AssetA and AssetB
      -- should be selected to cover the required quantity of AssetA
      , 50 /\ [ AssetA /\ 6, AssetB /\ 3 ]
      -- ^ pair for AssetA and AssetB
      -- should be selected to cover the required quantity of AssetA
      , 70 /\ mempty
      -- ^ singleton for AssetLovelace
      -- should be selected to cover the required quantity of AssetLovelace
      , 100 /\ [ AssetA /\ 10, AssetB /\ 1, AssetC /\ 1 ]
      -- ^ bundle containing multiple assets including AssetA
      -- should not be selected
      , 50 /\ [ AssetB /\ 1 ]
      -- ^ singleton for AssetB - should not be selected
      ]
  , selectedUtxos:
      [ 50 /\ [ AssetA /\ 4, AssetB /\ 1 ]
      , 50 /\ [ AssetA /\ 6, AssetB /\ 3 ]
      , 70 /\ mempty
      ]
  }

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
