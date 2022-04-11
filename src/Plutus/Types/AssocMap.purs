module Plutus.Types.AssocMap
  ( Map
  , lookup
  ) where

import Prelude

import Data.Array (find)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(Nothing))
import Data.Foldable (class Foldable, foldlDefault, foldMap, foldrDefault)
import Data.Traversable (class Traversable, for, sequence, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Map)) as PD

-- | A Plutus-style associated list `Map` of key-value pairs.
newtype Map (k :: Type) (v :: Type) = Map { unMap :: Array (Tuple k v) }

derive instance (Eq k, Eq v) => Eq (Map k v)
derive instance (Ord k, Ord v) => Ord (Map k v)

instance (Show k, Show v) => Show (Map k v) where
  show (Map { unMap }) = "(" <> "Map " <> show unMap <> ")"

instance (ToData k, ToData v) => ToData (Map k v) where
  toData (Map { unMap }) = PD.Map (bimap toData toData <$> unMap)

instance (FromData k, FromData v) => FromData (Map k v) where
  fromData (PD.Map mp) = do
    Map <$>
      ( { unMap: _ } <$> for mp \(k /\ v) ->
          Tuple <$> fromData k <*> fromData v
      )
  fromData _ = Nothing

instance Functor (Map k) where
  map f (Map { unMap }) = Map { unMap: map (map f) unMap }

instance Foldable (Map k) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (Map { unMap }) = foldMap (foldMap f) unMap

instance Traversable (Map k) where
  traverse f (Map { unMap }) =
    Map <$> ({ unMap: _ } <$> traverse (traverse f) unMap)
  sequence (Map { unMap }) =
    Map <$> ({ unMap: _ } <$> sequence (map sequence unMap))

-- | Find an entry in a 'Map'.
lookup :: forall (k :: Type) (v :: Type). Eq k => k -> Map k v -> Maybe v
lookup c (Map { unMap }) = find (fst >>> (==) c) unMap <#> snd