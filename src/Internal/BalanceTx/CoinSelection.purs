module Ctl.Internal.BalanceTx.CoinSelection where

import Prelude

import Ctl.Internal.BalanceTx.Types (BalanceTxM)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Cardano.Types.Value (Coin, CurrencySymbol, Value(Value))
import Ctl.Internal.Cardano.Types.Value (flattenNonAdaValue) as Value
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array ((!!))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (cons', fromArray, singleton, uncons) as NEArray
import Data.Function (applyFlipped)
import Data.Map (Map)
import Data.Map (delete, filter, isEmpty, size, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe')
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, isEmpty, member, singleton, size) as Set
import Data.Tuple.Nested (type (/\), uncurry2, (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt) as Random
import Type.Proxy (Proxy)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- CoinSelection
--------------------------------------------------------------------------------

class CoinSelection (selection :: Type) where
  performSelection
    :: Proxy selection -> UtxoMap -> Value -> BalanceTxM (Set TransactionInput)

data MultiAssetSelection (strategy :: Type)

data SelectionStrategyMinimal

instance CoinSelection (MultiAssetSelection SelectionStrategyMinimal) where
  performSelection _ utxos requiredValue = undefined

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

-- https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L460
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

-- https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L399
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

-- https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L418
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

-- https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L598
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

--------------------------------------------------------------------------------
-- AssetClass
--------------------------------------------------------------------------------

data AssetClass = AssetClass CurrencySymbol TokenName

derive instance Eq AssetClass
derive instance Ord AssetClass

valueAssetClasses :: Value -> Set AssetClass
valueAssetClasses (Value _ assets) =
  Set.fromFoldable $ uncurry2 AssetClass <$> Value.flattenNonAdaValue assets

txOutputAssetClasses :: TransactionOutput -> Set AssetClass
txOutputAssetClasses = valueAssetClasses <<< _.amount <<< unwrap

