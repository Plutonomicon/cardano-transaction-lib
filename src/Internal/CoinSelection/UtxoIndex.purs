module Ctl.Internal.CoinSelection.UtxoIndex
  ( Asset(Asset, AssetLovelace)
  , SelectionFilter(SelectAnyWith, SelectPairWith, SelectSingleton)
  , TxUnspentOutput
  , UtxoIndex
  , UtxoIndexInvariantStatus
      ( InvariantHolds
      , InvariantUtxoIndexIncomplete
      , InvariantUtxoIndexNonMinimal
      )
  , buildUtxoIndex
  , checkUtxoIndexInvariants
  , emptyUtxoIndex
  , selectRandomWithFilter
  , utxoIndexPartition
  , utxoIndexDeleteEntry
  , utxoIndexInsertEntry
  , utxoIndexUniverse
  , utxoIndexDisjoint
  , valueHasAsset
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  )
import Ctl.Internal.Cardano.Types.Value (AssetClass, Value)
import Ctl.Internal.Cardano.Types.Value
  ( getAssetQuantity
  , valueAssetClasses
  , valueAssets
  , valueToCoin'
  ) as Value
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (all, foldl) as Array
import Data.Array ((!!))
import Data.Array.NonEmpty (cons')
import Data.Bifunctor (bimap)
import Data.Foldable (all, length) as Foldable
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.List (List)
import Data.Map (Map)
import Data.Map
  ( alter
  , delete
  , empty
  , insert
  , intersection
  , isEmpty
  , lookup
  , singleton
  , size
  , toUnfoldable
  , update
  ) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, toUnfoldable) as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt) as Random
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Type.Proxy (Proxy(Proxy))

-- | A utxo set that is indexed by asset identifier.
-- | The index provides a mapping from assets to subsets of the utxo set.
-- |
-- | The index makes it possible to efficiently compute the subset of a utxo set
-- | containing a particular asset, or to select just a single utxo containing a
-- | particular asset, without having to search linearly through the entire set
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L176
type UtxoIndexRec =
  { indexAnyWith :: Map Asset UtxoMap
  -- ^ An index of all utxos that contain the given asset.
  , indexSingletons :: Map Asset UtxoMap
  -- ^ An index of all utxos that contain the given asset and no other assets.
  , indexPairs :: Map Asset UtxoMap
  -- ^ An index of all utxos that contain the given asset and exactly one
  -- other asset.
  , utxos :: UtxoMap
  -- ^ The complete set of all utxos.
  }

newtype UtxoIndex = UtxoIndex UtxoIndexRec

derive instance Eq UtxoIndex

instance Show UtxoIndex where
  show (UtxoIndex utxoIndex) = "(UtxoIndex " <> show utxoIndex <> ")"

utxoIndexUniverse :: UtxoIndex -> UtxoMap
utxoIndexUniverse = view _utxos

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L485
data Asset = AssetLovelace | Asset AssetClass

derive instance Generic Asset _
derive instance Eq Asset
derive instance Ord Asset

instance Show Asset where
  show = genericShow

instance Arbitrary Asset where
  arbitrary = oneOf $ cons' (pure AssetLovelace) [ Asset <$> arbitrary ]

-- | Indicates whether or not a given bundle includes a given asset.
-- |
-- | Both ada and non-ada assets can be queried.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L526
valueHasAsset :: Value -> Asset -> Boolean
valueHasAsset amount AssetLovelace =
  (Value.valueToCoin' amount) > (BigInt.fromInt 0)
valueHasAsset amount (Asset asset) =
  Value.getAssetQuantity asset amount >= one

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

-- | An index with no entries.
emptyUtxoIndex :: UtxoIndex
emptyUtxoIndex = UtxoIndex
  { indexAnyWith: Map.empty
  , indexSingletons: Map.empty
  , indexPairs: Map.empty
  , utxos: Map.empty
  }

-- | Creates an index from a `UtxoMap`.
buildUtxoIndex :: UtxoMap -> UtxoIndex
buildUtxoIndex =
  Array.foldl (flip utxoIndexInsertEntry) emptyUtxoIndex <<< Map.toUnfoldable

-- | Partition `UtxoIndex`
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L344
utxoIndexPartition
  :: (TransactionInput -> Boolean) -> UtxoIndex -> (UtxoIndex /\ UtxoIndex)
utxoIndexPartition predicate =
  bimap buildUtxoIndex buildUtxoIndex <<< partitionMapOnKeys predicate
    <<< utxoIndexUniverse

partitionMapOnKeys
  :: forall k v. Ord k => (k -> Boolean) -> Map k v -> Map k v /\ Map k v
partitionMapOnKeys p m =
  foldl select (Map.empty /\ Map.empty) (Map.toUnfoldable m :: List (k /\ v))
  where
  select :: (Map k v /\ Map k v) -> (k /\ v) -> (Map k v /\ Map k v)
  select (yes /\ no) (k /\ v) =
    if p k then (Map.insert k v yes) /\ no
    else yes /\ (Map.insert k v no)

--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L561
utxoIndexInsertEntry :: TxUnspentOutput -> UtxoIndex -> UtxoIndex
utxoIndexInsertEntry (oref /\ out) =
  (_utxos %~ Map.insert oref out) <<< updateUtxoIndex out insertEntry
  where
  insertEntry :: Asset -> Map Asset UtxoMap -> Map Asset UtxoMap
  insertEntry =
    Map.alter
      (Just <<< maybe (Map.singleton oref out) (Map.insert oref out))

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L295
utxoIndexDeleteEntry :: TxUnspentOutput -> UtxoIndex -> UtxoIndex
utxoIndexDeleteEntry (inp /\ out) =
  (_utxos %~ Map.delete inp) <<< updateUtxoIndex out deleteEntry
  where
  deleteEntry :: Asset -> Map Asset UtxoMap -> Map Asset UtxoMap
  deleteEntry = Map.update (Just <<< Map.delete inp)

updateUtxoIndex
  :: TransactionOutput
  -> (Asset -> Map Asset UtxoMap -> Map Asset UtxoMap)
  -> UtxoIndex
  -> UtxoIndex
updateUtxoIndex out manageEntry =
  case categorizeUtxoEntry out of
    BundleWithNoAssets ->
      _indexSingletons %~ manageEntry AssetLovelace
    BundleWithOneAsset asset ->
      (_indexPairs %~ manageEntry AssetLovelace)
        <<< (_indexSingletons %~ manageEntry (Asset asset))
    BundleWithTwoAssets asset0 asset1 ->
      (_indexAnyWith %~ manageEntry AssetLovelace)
        <<< (_indexPairs %~ manageEntry (Asset asset0))
        <<< (_indexPairs %~ manageEntry (Asset asset1))
    BundleWithMultipleAssets assets ->
      (_indexAnyWith %~ flip (foldl (flip (manageEntry <<< Asset))) assets)
        <<< (_indexAnyWith %~ manageEntry AssetLovelace)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L537
data BundleCategory
  = BundleWithNoAssets
  | BundleWithOneAsset AssetClass
  | BundleWithTwoAssets AssetClass AssetClass
  | BundleWithMultipleAssets (Set AssetClass)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L546
categorizeUtxoEntry :: TransactionOutput -> BundleCategory
categorizeUtxoEntry txOutput = case Set.toUnfoldable bundleAssets of
  [] -> BundleWithNoAssets
  [ a ] -> BundleWithOneAsset a
  [ a, b ] -> BundleWithTwoAssets a b
  _ -> BundleWithMultipleAssets bundleAssets
  where
  bundleAssets :: Set AssetClass
  bundleAssets = txOutputAssetClasses txOutput

--------------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------------

-- | Indicates whether a pair of UTxO indices are disjoint.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L390
utxoIndexDisjoint :: UtxoIndex -> UtxoIndex -> Boolean
utxoIndexDisjoint x y = Map.isEmpty $ on Map.intersection utxoIndexUniverse x y

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

type TxUnspentOutput = TransactionInput /\ TransactionOutput

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/3d722b27f6dbd2cf05da497297e60e3b54b1ef6e/lib/wallet/src/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L399
data SelectionFilter
  = SelectSingleton Asset
  | SelectPairWith Asset
  | SelectAnyWith Asset

instance Arbitrary SelectionFilter where
  arbitrary = oneOf $
    cons' (SelectSingleton <$> arbitrary)
      [ (SelectPairWith <$> arbitrary), (SelectAnyWith <$> arbitrary) ]

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L418
selectRandomWithFilter
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => UtxoIndex
  -> SelectionFilter
  -> m (Maybe (TxUnspentOutput /\ UtxoIndex))
selectRandomWithFilter utxoIndex selectionFilter =
  selectRandomMapMember selectionUtxoMap
    <#> map (\utxo -> utxo /\ utxoIndexDeleteEntry utxo utxoIndex)
  where
  selectionUtxoMap :: UtxoMap
  selectionUtxoMap =
    case selectionFilter of
      SelectSingleton asset ->
        asset `lookupWith` _indexSingletons
      SelectPairWith asset ->
        asset `lookupWith` _indexPairs
      SelectAnyWith asset ->
        asset `lookupWith` _indexAnyWith
    where
    lookupWith :: Asset -> Lens' UtxoIndex (Map Asset UtxoMap) -> UtxoMap
    lookupWith asset getter =
      fromMaybe Map.empty $ Map.lookup asset (utxoIndex ^. getter)

--------------------------------------------------------------------------------
-- Lenses for accessing `UtxoIndex` fields
--------------------------------------------------------------------------------

_UtxoIndex :: Iso' UtxoIndex UtxoIndexRec
_UtxoIndex = iso (\(UtxoIndex rec) -> rec) UtxoIndex

_indexAnyWith :: Lens' UtxoIndex (Map Asset UtxoMap)
_indexAnyWith = _UtxoIndex <<< prop (Proxy :: Proxy "indexAnyWith")

_indexSingletons :: Lens' UtxoIndex (Map Asset UtxoMap)
_indexSingletons = _UtxoIndex <<< prop (Proxy :: Proxy "indexSingletons")

_indexPairs :: Lens' UtxoIndex (Map Asset UtxoMap)
_indexPairs = _UtxoIndex <<< prop (Proxy :: Proxy "indexPairs")

_utxos :: Lens' UtxoIndex UtxoMap
_utxos = _UtxoIndex <<< prop (Proxy :: Proxy "utxos")

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

txOutputAssetClasses :: TransactionOutput -> Set AssetClass
txOutputAssetClasses =
  Set.fromFoldable <<< Value.valueAssetClasses <<< _.amount <<< unwrap

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

--------------------------------------------------------------------------------
-- Invariant
--------------------------------------------------------------------------------

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L614
data UtxoIndexInvariantStatus
  = InvariantHolds
  -- ^ Indicates a successful check of the invariants.
  | InvariantUtxoIndexIncomplete
  -- ^ Indicates that the `UtxoIndex` is missing one or more entries.
  | InvariantUtxoIndexNonMinimal

-- ^ Indicates that the `UtxoIndex` has one or more unnecessary entries.

derive instance Generic UtxoIndexInvariantStatus _

instance Show UtxoIndexInvariantStatus where
  show = genericShow

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L632
checkUtxoIndexInvariants :: UtxoIndex -> UtxoIndexInvariantStatus
checkUtxoIndexInvariants utxoIndex
  | not (checkUtxoIndexComplete utxoIndex) =
      InvariantUtxoIndexIncomplete
  | not (checkUtxoIndexMinimal utxoIndex) =
      InvariantUtxoIndexNonMinimal
  | otherwise =
      InvariantHolds

-- | Check that every entry from the map of all utxos is properly indexed.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L682
checkUtxoIndexComplete :: UtxoIndex -> Boolean
checkUtxoIndexComplete utxoIndex =
  Array.all hasEntries (Map.toUnfoldable $ utxoIndex ^. _utxos)
  where
  hasEntries :: TransactionInput /\ TransactionOutput -> Boolean
  hasEntries (oref /\ out) =
    case categorizeUtxoEntry out of
      BundleWithNoAssets ->
        _indexSingletons `hasEntryForAsset` AssetLovelace
      BundleWithOneAsset asset ->
        _indexPairs `hasEntryForAsset` AssetLovelace
          && _indexSingletons `hasEntryForAsset` Asset asset
      BundleWithTwoAssets asset0 asset1 ->
        _indexAnyWith `hasEntryForAsset` AssetLovelace
          && _indexPairs `hasEntryForAsset` Asset asset0
          && _indexPairs `hasEntryForAsset` Asset asset1
      BundleWithMultipleAssets assets ->
        _indexAnyWith `hasEntryForAsset` AssetLovelace &&
          flip Foldable.all assets \asset ->
            (_indexAnyWith `hasEntryForAsset` Asset asset)
    where
    hasEntryForAsset
      :: Lens' UtxoIndex (Map Asset UtxoMap) -> Asset -> Boolean
    hasEntryForAsset getter asset =
      maybe false (eq out) $
        (Map.lookup oref =<< Map.lookup asset (utxoIndex ^. getter))

-- | Check that every indexed entry is required by some entry in the map of all
-- | utxos.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/791541da69b9b3f434bb9ead43de406cc18b0373/lib/primitive/lib/Cardano/Wallet/Primitive/Types/UTxOIndex/Internal.hs#L715
checkUtxoIndexMinimal :: UtxoIndex -> Boolean
checkUtxoIndexMinimal utxoIndex =
  _indexSingletons `testEntriesWith` txOutputHasOneAsset
    && _indexPairs `testEntriesWith` txOutputHasTwoAssetsWith
    && _indexAnyWith `testEntriesWith` txOutputHasAsset
  where
  testEntriesWith
    :: Lens' UtxoIndex (Map Asset UtxoMap)
    -> (TransactionOutput -> Asset -> Boolean)
    -> Boolean
  testEntriesWith subset test' =
    utxoIndex ^. subset
      # Map.toUnfoldable
      # Array.all \(asset /\ utxos) ->
          Array.all (entryMatches (flip test' asset)) (Map.toUnfoldable utxos)
    where
    entryMatches
      :: (TransactionOutput -> Boolean)
      -> TransactionInput /\ TransactionOutput
      -> Boolean
    entryMatches test'' (oref /\ _) =
      maybe false test'' $
        Map.lookup oref (utxoIndexUniverse utxoIndex)

  txOutputHasOneAsset :: TransactionOutput -> Asset -> Boolean
  txOutputHasOneAsset txOutput AssetLovelace =
    txOutputAssetCount txOutput == zero
  txOutputHasOneAsset txOutput asset =
    txOutputHasAsset txOutput asset && txOutputAssetCount txOutput == one

  txOutputHasTwoAssetsWith :: TransactionOutput -> Asset -> Boolean
  txOutputHasTwoAssetsWith txOutput AssetLovelace =
    txOutputAssetCount txOutput == one
  txOutputHasTwoAssetsWith txOutput asset =
    txOutputHasAsset txOutput asset
      && txOutputAssetCount txOutput == BigInt.fromInt 2

  txOutputHasAsset :: TransactionOutput -> Asset -> Boolean
  txOutputHasAsset _ AssetLovelace = true
  txOutputHasAsset (TransactionOutput { amount }) (Asset asset) =
    Value.getAssetQuantity asset amount >= one

  txOutputAssetCount :: TransactionOutput -> BigInt
  txOutputAssetCount =
    Foldable.length <<< Value.valueAssets <<< _.amount <<< unwrap
