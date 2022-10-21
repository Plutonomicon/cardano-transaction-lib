-- | This module provides a multi-asset coin selection algorithm replicated from
-- | cardano-wallet (https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs). The algorithm supports two selection
-- | strategies (optimal and minimal) and uses priority ordering and round-robin
-- | processing to handle the problem of over-selection.
module Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionState(SelectionState)
  , SelectionStrategy(SelectionStrategyMinimal, SelectionStrategyOptimal)
  , _leftoverUtxos
  , performMultiAssetSelection
  , selectedInputs
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( BalanceInsufficientError
      , InsufficientUtxoBalanceToCoverAsset
      )
  , Expected(Expected)
  , ImpossibleError(Impossible)
  )
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Cardano.Types.Value (AssetClass(AssetClass), Coin, Value)
import Ctl.Internal.Cardano.Types.Value
  ( getAssetQuantity
  , getCurrencySymbol
  , leq
  , valueAssetClasses
  , valueAssets
  , valueToCoin
  , valueToCoin'
  ) as Value
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.TokenName (getTokenName) as TokenName
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (snoc, uncons) as Array
import Data.Array ((!!))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (cons', fromArray, singleton, uncons) as NEArray
import Data.BigInt (BigInt)
import Data.BigInt (abs, fromInt, toString) as BigInt
import Data.Foldable (foldMap) as Foldable
import Data.Function (applyFlipped)
import Data.Lens (Lens')
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (fromFoldable, isEmpty, member, singleton, size) as Set
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt) as Random
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
  -> UtxoMap
  -> Value
  -> m SelectionState
performMultiAssetSelection strategy utxos requiredValue =
  case requiredValue `Value.leq` availableValue of
    true ->
      runRoundRobinM (mkSelectionState utxos) selectors
    false ->
      throwError balanceInsufficientError
  where
  balanceInsufficientError :: BalanceTxError
  balanceInsufficientError =
    BalanceInsufficientError (Expected requiredValue) (Actual availableValue)

  availableValue :: Value
  availableValue = balance utxos

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
  { leftoverUtxos :: UtxoMap
  , selectedUtxos :: UtxoMap
  }

derive instance Newtype SelectionState _

_leftoverUtxos :: Lens' SelectionState UtxoMap
_leftoverUtxos = _Newtype <<< prop (Proxy :: Proxy "leftoverUtxos")

_selectedUtxos :: Lens' SelectionState UtxoMap
_selectedUtxos = _Newtype <<< prop (Proxy :: Proxy "selectedUtxos")

-- | Creates an initial `SelectionState` where none of the utxos are selected.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L192
mkSelectionState :: UtxoMap -> SelectionState
mkSelectionState = wrap <<< { leftoverUtxos: _, selectedUtxos: Map.empty }

-- | Moves a single utxo entry from the leftover set to the selected set.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOSelection.hs#L426
selectUtxo :: TxUnspentOutput -> SelectionState -> SelectionState
selectUtxo (oref /\ out) =
  over _selectedUtxos (Map.insert oref out)
    <<< over _leftoverUtxos (Map.delete oref)

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
      selectQuantityOf assetClass SelectionPriorityCover
  , selectQuantityImprove:
      selectQuantityOf assetClass SelectionPriorityImprove
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
      selectQuantityOf coin SelectionPriorityCover
  , selectQuantityImprove:
      selectQuantityOf coin SelectionPriorityImprove
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
  :: forall (m :: Type -> Type) (asset :: Type)
   . MonadEffect m
  => ApplySelectionFilter asset
  => asset
  -> SelectionPriority
  -> SelectionState
  -> m (Maybe SelectionState)
selectQuantityOf asset priority state =
  map updateState <$>
    selectRandomWithPriority (state ^. _leftoverUtxos) filters
  where
  filters :: NonEmptyArray (SelectionFilter asset)
  filters = filtersForAssetWithPriority asset priority

  updateState :: TxUnspentOutput /\ UtxoMap -> SelectionState
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
          InsufficientUtxoBalanceToCoverAsset Impossible lens.assetDisplayString
      in
        lens.selectQuantityCover state
          >>= maybe (throwError balanceInsufficientError) (pure <<< Just)
  | otherwise =
      -- Note that if the required asset quantity has already been reached,
      -- we attempt to improve the selection using `SelectionPriorityImprove`,
      -- which allows us to select only utxos containing the given asset and no
      -- other asset, i.e. we select from the "singleton" subset of utxos.
      bindFlipped requireImprovement <$> lens.selectQuantityImprove state
      where
      requireImprovement :: SelectionState -> Maybe SelectionState
      requireImprovement state'
        | distanceFromTarget state' < distanceFromTarget state = Just state'
        | otherwise = Nothing

      distanceFromTarget :: SelectionState -> BigInt
      distanceFromTarget =
        BigInt.abs <<< sub targetQuantity <<< lens.currentQuantity

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

type Processor (m :: Type -> Type) (s :: Type) (s' :: Type) = s -> m (Maybe s')

runRoundRobinM
  :: forall (m :: Type -> Type) (s :: Type)
   . Monad m
  => s
  -> Array (Processor m s s)
  -> m s
runRoundRobinM state = runRoundRobinM' state identity

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/a61d37f2557b8cb5c47b57da79375afad698eed4/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L2155
runRoundRobinM'
  :: forall (m :: Type -> Type) (s :: Type) (s' :: Type)
   . Monad m
  => s
  -> (s' -> s)
  -> Array (Processor m s s')
  -> m s
runRoundRobinM' state demote processors = go state processors []
  where
  go :: s -> Array (Processor m s s') -> Array (Processor m s s') -> m s
  go s [] [] = pure s
  go s ps qs =
    case Array.uncons ps of
      Nothing -> go s qs []
      Just { head: p, tail: ps' } ->
        p s >>= case _ of
          Nothing -> go s ps' qs
          Just s' -> go (demote s') ps' (Array.snoc qs p)

--------------------------------------------------------------------------------
-- SelectionPriority
--------------------------------------------------------------------------------

data SelectionPriority = SelectionPriorityCover | SelectionPriorityImprove

filtersForAssetWithPriority
  :: forall (asset :: Type)
   . asset
  -> SelectionPriority
  -> NonEmptyArray (SelectionFilter asset)
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
  :: forall (m :: Type -> Type) (asset :: Type)
   . MonadEffect m
  => ApplySelectionFilter asset
  => UtxoMap
  -> NonEmptyArray (SelectionFilter asset)
  -> m (Maybe (TxUnspentOutput /\ UtxoMap))
selectRandomWithPriority utxos filters =
  NEArray.uncons filters # \{ head: filter, tail } ->
    case NEArray.fromArray tail of
      Nothing ->
        selectRandomWithFilter utxos filter
      Just xs ->
        maybe' (\_ -> selectRandomWithPriority utxos xs) (pure <<< Just)
          =<< selectRandomWithFilter utxos filter

--------------------------------------------------------------------------------
-- SelectionFilter, ApplySelectionFilter
--------------------------------------------------------------------------------

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L399
data SelectionFilter (asset :: Type)
  = SelectSingleton asset
  | SelectPairWith asset
  | SelectAnyWith asset

class ApplySelectionFilter (asset :: Type) where
  applySelectionFilter :: UtxoMap -> SelectionFilter asset -> UtxoMap

instance ApplySelectionFilter AssetClass where
  applySelectionFilter utxos filter =
    flip Map.filter utxos $
      case filter of
        SelectSingleton asset ->
          eq (Set.singleton asset) <<< txOutputAssetClasses
        SelectPairWith asset ->
          (\assets -> Set.member asset assets && Set.size assets == 2)
            <<< txOutputAssetClasses
        SelectAnyWith asset ->
          Set.member asset <<< txOutputAssetClasses

instance ApplySelectionFilter Coin where
  applySelectionFilter utxos filter =
    case filter of
      SelectSingleton _ ->
        Map.filter (Set.isEmpty <<< txOutputAssetClasses) utxos
      SelectPairWith _ ->
        Map.filter (eq one <<< Set.size <<< txOutputAssetClasses) utxos
      SelectAnyWith _ -> utxos

type TxUnspentOutput = TransactionInput /\ TransactionOutput

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L418
selectRandomWithFilter
  :: forall (m :: Type -> Type) (asset :: Type)
   . MonadEffect m
  => ApplySelectionFilter asset
  => UtxoMap
  -> SelectionFilter asset
  -> m (Maybe (TxUnspentOutput /\ UtxoMap))
selectRandomWithFilter utxos filter =
  selectRandomMapMember (applySelectionFilter utxos filter)
    <#> map (\utxo@(oref /\ _) -> utxo /\ Map.delete oref utxos)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L598
selectRandomMapMember
  :: forall (m :: Type -> Type) (k :: Type) (v :: Type)
   . MonadEffect m
  => Map k v
  -> m (Maybe (k /\ v))
selectRandomMapMember m
  | Map.isEmpty m = pure Nothing
  | otherwise = liftEffect do
      idx <- Random.randomInt zero (Map.size m - one)
      pure $ Map.toUnfoldable m !! idx

txOutputAssetClasses :: TransactionOutput -> Set AssetClass
txOutputAssetClasses =
  Set.fromFoldable <<< Value.valueAssetClasses <<< _.amount <<< unwrap

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

