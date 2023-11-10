-- | This module provides a multi-asset coin selection algorithm replicated from
-- | cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3395b6e4749544d552125dfd0e060437b5c18d5c/lib/coin-selection/lib/Cardano/CoinSelection/Balance.hs
-- |
-- | The algorithm supports two selection strategies (optimal and minimal) and
-- | uses priority ordering and round-robin processing to handle the problem
-- | of over-selection.
module Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionState(SelectionState)
  , SelectionStrategy(SelectionStrategyMinimal, SelectionStrategyOptimal)
  , _leftoverUtxos
  , empty
  , fromIndexFiltered
  , mkSelectionState
  , performMultiAssetSelection
  , runRoundRobinM -- Exported for tests
  , selectedInputs
  , selectUtxo
  , selectRandomWithPriority
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( BalanceInsufficientError
      , InsufficientUtxoBalanceToCoverAsset
      )
  , Expected(Expected)
  )
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.Value (AssetClass(AssetClass), Coin, Value)
import Ctl.Internal.Cardano.Types.Value
  ( getAssetQuantity
  , getCurrencySymbol
  , leq
  , valueAssets
  , valueToCoin
  , valueToCoin'
  ) as Value
import Ctl.Internal.CoinSelection.UtxoIndex
  ( Asset(Asset, AssetLovelace)
  , SelectionFilter(SelectAnyWith, SelectPairWith, SelectSingleton)
  , TxUnspentOutput
  , UtxoIndex
  , emptyUtxoIndex
  , selectRandomWithFilter
  , utxoIndexDeleteEntry
  , utxoIndexPartition
  , utxoIndexUniverse
  )
import Ctl.Internal.Plutus.Conversion (toPlutusValue)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.TokenName (getTokenName) as TokenName
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (snoc, uncons) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (cons', fromArray, singleton, uncons) as NEArray
import Data.Foldable (foldMap) as Foldable
import Data.Function (applyFlipped)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (fromFoldable) as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toString) as BigInt
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (elements) as Arbitrary
import Type.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- Coin Selection
--------------------------------------------------------------------------------

-- | A `SelectionStrategy` determines how much of each asset the selection
-- | algorithm will attempt to select from the available utxo set.
-- |
-- | Specifying `SelectionStrategyOptimal` will cause the selection algorithm to
-- | attempt to select around twice the required amount of each asset from the
-- | available utxo set, making it possible to generate change outputs that are
-- | roughly the same sizes and shapes as the user-specified outputs. Using this
-- | strategy will help to ensure that a wallet's utxo distribution can evolve
-- | over time to resemble the typical distribution of payments made by the
-- | wallet owner.
-- |
-- | Specifying `SelectionStrategyMinimal` will cause the selection algorithm to
-- | only select just enough of each asset from the available utxo set to meet
-- | the required amount. It is advised to use this strategy only when
-- | necessary, as it increases the likelihood of generating change outputs that
-- | are much smaller than user-specified outputs. If this strategy is used
-- | regularly, the utxo set can evolve to a state where the distribution no
-- | longer resembles the typical distribution of payments made by the user.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L325
data SelectionStrategy = SelectionStrategyOptimal | SelectionStrategyMinimal

instance Arbitrary SelectionStrategy where
  arbitrary =
    Arbitrary.elements $
      NEArray.cons' SelectionStrategyOptimal [ SelectionStrategyMinimal ]

-- | Performs a coin selection using the specified selection strategy.
-- |
-- | Throws a `BalanceInsufficientError` if the balance of the provided utxo
-- | set is insufficient to cover the balance required.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1128
performMultiAssetSelection
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow BalanceTxError m
  => SelectionStrategy
  -> UtxoIndex
  -> Value
  -> m SelectionState
performMultiAssetSelection strategy utxoIndex requiredValue =
  case requiredValue `Value.leq` availableValue of
    true ->
      runRoundRobinM (mkSelectionState utxoIndex) selectors
    false ->
      throwError balanceInsufficientError
  where
  balanceInsufficientError :: BalanceTxError
  balanceInsufficientError =
    BalanceInsufficientError
      (Expected $ toPlutusValue requiredValue)
      (Actual $ toPlutusValue availableValue)

  availableValue :: Value
  availableValue = balance (utxoIndexUniverse utxoIndex)

  selectors
    :: Array (SelectionState -> m (Maybe SelectionState))
  selectors = map assetSelector assets `Array.snoc` coinSelector
    where
    assets :: Array (AssetClass /\ BigInt)
    assets = Value.valueAssets requiredValue

    assetSelector
      :: AssetClass /\ BigInt -> SelectionState -> m (Maybe SelectionState)
    assetSelector = runSelectionStep <<< assetSelectionLens strategy

    coinSelector :: SelectionState -> m (Maybe SelectionState)
    coinSelector =
      runSelectionStep $
        coinSelectionLens strategy (Value.valueToCoin requiredValue)

-- | Represents the internal state of a selection.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L145
newtype SelectionState = SelectionState
  { leftoverUtxos :: UtxoIndex
  , selectedUtxos :: UtxoMap
  }

derive instance Generic SelectionState _
derive instance Newtype SelectionState _
derive instance Eq SelectionState

instance Show SelectionState where
  show = genericShow

_leftoverUtxos :: Lens' SelectionState UtxoIndex
_leftoverUtxos = _Newtype <<< prop (Proxy :: Proxy "leftoverUtxos")

_selectedUtxos :: Lens' SelectionState UtxoMap
_selectedUtxos = _Newtype <<< prop (Proxy :: Proxy "selectedUtxos")

-- | Creates an initial `SelectionState` where none of the utxos are selected.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L192
mkSelectionState :: UtxoIndex -> SelectionState
mkSelectionState = wrap <<< { leftoverUtxos: _, selectedUtxos: Map.empty }

-- | A completely empty selection with no selected or leftover UTxOs.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L183
empty :: SelectionState
empty = mkSelectionState emptyUtxoIndex

fromIndexFiltered
  :: (TransactionInput -> Boolean) -> UtxoIndex -> SelectionState
fromIndexFiltered predicate index = SelectionState
  { leftoverUtxos: no, selectedUtxos: utxoIndexUniverse yes }
  where
  yes /\ no = utxoIndexPartition predicate index

-- | Moves a single utxo entry from the leftover set to the selected set.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L426
selectUtxo :: TxUnspentOutput -> SelectionState -> SelectionState
selectUtxo utxo@(oref /\ out) =
  over _selectedUtxos (Map.insert oref out)
    <<< over _leftoverUtxos (utxoIndexDeleteEntry utxo)

-- | Returns the balance of the given utxo set.
balance :: UtxoMap -> Value
balance = Foldable.foldMap (_.amount <<< unwrap)

-- | Returns the balance of selected utxos.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L375
selectedBalance :: SelectionState -> Value
selectedBalance = balance <<< _.selectedUtxos <<< unwrap

-- | Returns the quantity of the given asset in the selected `Value`.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L2169
selectedAssetQuantity :: AssetClass -> SelectionState -> BigInt
selectedAssetQuantity assetClass =
  Value.getAssetQuantity assetClass <<< selectedBalance

-- | Returns the selected amount of Ada.
-- |
-- | Taken from cardano-wallet:
-- |https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L2175
selectedCoinQuantity :: SelectionState -> BigInt
selectedCoinQuantity = Value.valueToCoin' <<< selectedBalance

-- | Returns the output references of the selected utxos.
selectedInputs :: SelectionState -> Set TransactionInput
selectedInputs = Set.fromFoldable <<< Map.keys <<< view _selectedUtxos

-- | A `SelectionLens` gives `runSelectionStep` the information on the current
-- | selection state along with the functions required to transition to the next
-- | state.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1254
type SelectionLens (m :: Type -> Type) =
  { assetDisplayString :: String
  , currentQuantity :: SelectionState -> BigInt
  , requiredQuantity :: BigInt
  , selectQuantityCover :: SelectionState -> m (Maybe SelectionState)
  , selectQuantityImprove :: SelectionState -> m (Maybe SelectionState)
  , selectionStrategy :: SelectionStrategy
  }

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1159
assetSelectionLens
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => SelectionStrategy
  -> AssetClass /\ BigInt
  -> SelectionLens m
assetSelectionLens selectionStrategy (assetClass /\ requiredQuantity) =
  { assetDisplayString:
      showAssetClassWithQuantity assetClass requiredQuantity
  , currentQuantity:
      selectedAssetQuantity assetClass
  , requiredQuantity
  , selectQuantityCover:
      selectQuantityOf (Asset assetClass) SelectionPriorityCover
  , selectQuantityImprove:
      selectQuantityOf (Asset assetClass) SelectionPriorityImprove
  , selectionStrategy
  }

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1173
coinSelectionLens
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => SelectionStrategy
  -> Coin
  -> SelectionLens m
coinSelectionLens selectionStrategy coin =
  { assetDisplayString: show coin
  , currentQuantity: selectedCoinQuantity
  , requiredQuantity: unwrap coin
  , selectQuantityCover:
      selectQuantityOf AssetLovelace SelectionPriorityCover
  , selectQuantityImprove:
      selectQuantityOf AssetLovelace SelectionPriorityImprove
  , selectionStrategy
  }

-- | Selects an utxo entry that matches one of the filters derived from the
-- | given `SelectionPriority`. This function traverses the list of filters from
-- | left to right, in descending order of priority.
-- |
-- | Returns `Nothing` if it traverses the entire list of filters without
-- | successfully selecting an utxo entry.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1217
selectQuantityOf
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => Asset
  -> SelectionPriority
  -> SelectionState
  -> m (Maybe SelectionState)
selectQuantityOf asset priority state =
  map updateState <$>
    selectRandomWithPriority (state ^. _leftoverUtxos) filters
  where
  filters :: NonEmptyArray SelectionFilter
  filters = filtersForAssetWithPriority asset priority

  updateState :: TxUnspentOutput /\ UtxoIndex -> SelectionState
  updateState = flip selectUtxo state <<< Tuple.fst

-- | Runs just a single step of a coin selection.
-- |
-- | It returns an updated state if (and only if) the updated selection
-- | represents an improvement over the selection in the previous state.
-- |
-- | An improvement, for a given asset quantity, is defined as follows:
-- |
-- |  - If the total selected asset quantity of the previous selection had
-- |    not yet reached 100% of the output asset quantity, any additional
-- |    selection is considered to be an improvement.
-- |
-- |  - If the total selected asset quantity of the previous selection had
-- |    already reached or surpassed 100% of the output asset quantity, any
-- |    additional selection is considered to be an improvement if and only
-- |    if it takes the total selected asset quantity closer to the target
-- |    asset quantity, but not further away.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1284
runSelectionStep
  :: forall (m :: Type -> Type)
   . MonadThrow BalanceTxError m
  => SelectionLens m
  -> SelectionState
  -> m (Maybe SelectionState)
runSelectionStep lens state
  | lens.currentQuantity state < lens.requiredQuantity =
      let
        balanceInsufficientError :: BalanceTxError
        balanceInsufficientError =
          InsufficientUtxoBalanceToCoverAsset lens.assetDisplayString
      in
        lens.selectQuantityCover state
          >>= maybe (throwError balanceInsufficientError) (pure <<< Just)
  | otherwise =
      -- Note that if the required asset quantity has already been reached,
      -- we attempt to improve the selection using `SelectionPriorityImprove`,
      -- which allows us to select only utxos containing the given asset and no
      -- other asset, i.e. we select from the "singleton" subset of utxos.
      (requireImprovement =<< _) <$> lens.selectQuantityImprove state
      where
      requireImprovement :: SelectionState -> Maybe SelectionState
      requireImprovement state'
        | distanceFromTarget state' < distanceFromTarget state = Just state'
        | otherwise = Nothing

      distanceFromTarget :: SelectionState -> BigInt
      distanceFromTarget =
        bigIntAbs <<< sub targetQuantity <<< lens.currentQuantity

      bigIntAbs :: BigInt -> BigInt
      bigIntAbs x = if x < zero then negate x else x

      targetMultiplier :: Int
      targetMultiplier =
        case lens.selectionStrategy of
          SelectionStrategyMinimal -> 1
          SelectionStrategyOptimal -> 2

      targetQuantity :: BigInt
      targetQuantity =
        lens.requiredQuantity * (BigInt.fromInt targetMultiplier)

--------------------------------------------------------------------------------
-- Round-robin processing
--------------------------------------------------------------------------------

type Processor (m :: Type -> Type) (s :: Type) = s -> m (Maybe s)

-- | Uses given processors to update the state sequentially.
-- | Removes the processor from the list if applying it to the state returns
-- | `Nothing`. Each processor can only be applied once per round and is
-- | carried over to the next round if it has successfully updated the state.
-- |
-- | We use Round-robin processing to perform coin selection in multiple rounds,
-- | where a `Processor` runs a single selection step (`runSelectionStep`) for
-- | an asset from the set of all assets present in `requiredValue`.
-- | It returns `Nothing` in case the selection for a particular asset is
-- | already optimal and cannot be improved further.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3395b6e4749544d552125dfd0e060437b5c18d5c/lib/coin-selection/lib/Cardano/CoinSelection/Balance.hs#L2155
runRoundRobinM
  :: forall (m :: Type -> Type) (s :: Type)
   . Monad m
  => s
  -> Array (Processor m s)
  -> m s
runRoundRobinM state processors = go state processors []
  where
  go :: s -> Array (Processor m s) -> Array (Processor m s) -> m s
  go s [] [] = pure s
  go s ps qs =
    case Array.uncons ps of
      Nothing -> go s qs []
      Just { head: p, tail: ps' } ->
        p s >>= case _ of
          Nothing -> go s ps' qs
          Just s' -> go s' ps' (Array.snoc qs p)

--------------------------------------------------------------------------------
-- SelectionPriority
--------------------------------------------------------------------------------

data SelectionPriority = SelectionPriorityCover | SelectionPriorityImprove

filtersForAssetWithPriority
  :: Asset
  -> SelectionPriority
  -> NonEmptyArray SelectionFilter
filtersForAssetWithPriority asset priority =
  case priority of
    SelectionPriorityCover ->
      applyFlipped asset <$>
        NEArray.cons' SelectSingleton [ SelectPairWith, SelectAnyWith ]
    SelectionPriorityImprove ->
      NEArray.singleton (SelectSingleton asset)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L460
selectRandomWithPriority
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => UtxoIndex
  -> NonEmptyArray SelectionFilter
  -> m (Maybe (TxUnspentOutput /\ UtxoIndex))
selectRandomWithPriority utxoIndex filters =
  NEArray.uncons filters # \{ head: filter, tail } ->
    case NEArray.fromArray tail of
      Nothing ->
        selectRandomWithFilter utxoIndex filter
      Just xs ->
        maybe' (\_ -> selectRandomWithPriority utxoIndex xs) (pure <<< Just)
          =<< selectRandomWithFilter utxoIndex filter

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

showAssetClassWithQuantity :: AssetClass -> BigInt -> String
showAssetClassWithQuantity (AssetClass cs tn) quantity =
  "(Asset (" <> displayCurrencySymbol <> displayTokenName <> displayQuantity
  where
  displayCurrencySymbol :: String
  displayCurrencySymbol =
    "cs: " <> byteArrayToHex (Value.getCurrencySymbol cs) <> ", "

  displayTokenName :: String
  displayTokenName =
    "tn: " <> byteArrayToHex (TokenName.getTokenName tn) <> ", "

  displayQuantity :: String
  displayQuantity =
    "quantity: " <> BigInt.toString quantity <> "))"
