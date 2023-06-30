module Ctl.Internal.Plutus.Types.AssocMap
  ( Map(Map)
  , delete
  , elems
  , empty
  , filter
  , insert
  , keys
  , lookup
  , mapMaybe
  , mapMaybeWithKey
  , mapThese
  , member
  , null
  , singleton
  , union
  , unionWith
  , values
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , decodeAeson
  , encodeAeson
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.PlutusData (PlutusData(Map)) as PD
import Data.Array (any, deleteAt, filter, findIndex, mapMaybe, null, singleton) as Array
import Data.Array ((:))
import Data.Bifunctor (bimap, rmap)
import Data.Bitraversable (rtraverse)
import Data.Foldable
  ( class Foldable
  , foldMap
  , foldlDefault
  , foldr
  , foldrDefault
  )
import Data.Foldable (lookup) as Foldable
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldlWithIndexDefault
  , foldrWithIndexDefault
  )
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This), these)
import Data.Traversable (class Traversable, for, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))

-- Taken from local Hoogle server for the Haskell server which uses
-- `plutus` rev: 1efbb276ef1a10ca6961d0fd32e6141e9798bd11
-- It looks like Plutus doesn't provide any safety on their `Map`, notice the
-- existence of functions like `fromList :: [(k, v)] -> Map k v` and
-- toList :: Map k v -> [(k, v)]. Therefore, I will derive Generic and Newtype
-- instance and export the constructor. We could potentially provide this by
-- having a `fromList` in `Maybe` although we should probably try to replicate
-- behaviour so that our CTL and on-chain behaviours match. There doesn't even
-- seem to be an `Ord` constraint on the keys.
-- | A Plutus-style associated list `Map` of key-value pairs.
newtype Map (k :: Type) (v :: Type) = Map (Array (Tuple k v))

derive instance Generic (Map k v) _
derive instance Newtype (Map k v) _
derive newtype instance (Eq k, Eq v) => Eq (Map k v)
derive newtype instance (Ord k, Ord v) => Ord (Map k v)

instance (EncodeAeson k, EncodeAeson v) => EncodeAeson (Map k v) where
  encodeAeson = encodeAeson <<< map (rmap encodeAeson) <<< unwrap

instance (DecodeAeson k, DecodeAeson v) => DecodeAeson (Map k v) where
  decodeAeson x = Map <$>
    ( traverse (rtraverse decodeAeson)
        =<< (decodeAeson x :: _ (Array (Tuple k Aeson)))
    )

instance (Show k, Show v) => Show (Map k v) where
  show = genericShow

instance (ToData k, ToData v) => ToData (Map k v) where
  toData (Map xs) = PD.Map (bimap toData toData <$> xs)

instance (FromData k, FromData v) => FromData (Map k v) where
  fromData (PD.Map mp) = do
    Map <$>
      ( for mp \(k /\ v) ->
          Tuple <$> fromData k <*> fromData v
      )
  fromData _ = Nothing

instance Functor (Map k) where
  map f (Map xs) = Map $ map (map f) xs

instance FunctorWithIndex k (Map k) where
  mapWithIndex f (Map xs) = Map $ map (\(Tuple k v) -> Tuple k (f k v)) xs

instance Foldable (Map k) where
  foldMap f (Map xs) = foldMap (foldMap f) xs
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance FoldableWithIndex k (Map k) where
  foldMapWithIndex f (Map xs) = foldMap (uncurry f) xs
  foldrWithIndex f = foldrWithIndexDefault f
  foldlWithIndex f = foldlWithIndexDefault f

instance Traversable (Map k) where
  traverse f (Map xs) = Map <$> traverse (traverse f) xs
  sequence (Map xs) = Map <$> sequence (map sequence xs)

instance TraversableWithIndex k (Map k) where
  traverseWithIndex f (Map xs) = Map <$> traverse
    (\(Tuple k v) -> Tuple k <$> f k v)
    xs

instance (Eq k, Semigroup v) => Semigroup (Map k v) where
  append = unionWith (<>)

instance (Eq k, Semigroup v) => Monoid (Map k v) where
  mempty = empty

-- | Find an entry in a `Map`.
lookup :: forall (k :: Type) (v :: Type). Eq k => k -> Map k v -> Maybe v
lookup k (Map xs) = Foldable.lookup k xs

-- | Whether given key is a member of a `Map`
member :: forall (k :: Type) (v :: Type). Eq k => k -> Map k v -> Boolean
member k = isJust <<< lookup k

-- Insert a key-value pair into a `Map`
insert :: forall k v. (Eq k) => k -> v -> Map k v -> Map k v
insert k v m = unionWith (\_ b -> b) m $ singleton k v

-- | Delete a key in a `Map`
delete :: forall (k :: Type) (v :: Type). Eq k => k -> Map k v -> Map k v
delete k (Map xs) =
  case Array.findIndex (fst >>> (==) k) xs >>= flip Array.deleteAt xs of
    Nothing -> Map xs
    Just newMap -> Map newMap

-- | The keys of a `Map`
keys :: forall (k :: Type) (v :: Type). Map k v -> Array k
keys (Map xs) = fst <$> xs

-- | The values of a `Map`
values :: forall (k :: Type) (v :: Type). Map k v -> Array v
values (Map xs) = snd <$> xs

-- | Combine two `Map`s
union
  :: forall (k :: Type) (v :: Type) (r :: Type)
   . Eq k
  => Map k v
  -> Map k r
  -> Map k (These v r)
union (Map ls) (Map rs) =
  let
    f :: v -> Maybe r -> These v r
    f a = case _ of
      Nothing -> This a
      Just b -> Both a b

    ls' :: Array (k /\ These v r)
    ls' = map (\(c /\ i) -> (c /\ f i (lookup c (Map rs)))) ls

    rs' :: Array (k /\ r)
    rs' =
      Array.filter (\(c /\ _) -> not (Array.any (\(c' /\ _) -> c' == c) ls)) rs

    rs'' :: Array (k /\ These v r)
    rs'' = map (map That) rs'
  in
    Map (ls' <> rs'')

-- | Combine two `Map`s with the given combination function
unionWith
  :: forall (k :: Type) (a :: Type)
   . Eq k
  => (a -> a -> a)
  -> Map k a
  -> Map k a
  -> Map k a
unionWith merge ls rs = these identity identity merge <$> union ls rs

-- | A version of Haskell's `Data.Map.Lazy.mapEither` that works with `These`
mapThese
  :: forall (k :: Type) (v :: Type) (a :: Type) (b :: Type)
   . (v -> These a b)
  -> Map k v
  -> (Map k a /\ Map k b)
mapThese f mps = Map (fst mappedThese) /\ Map (snd mappedThese)
  where
  f'
    :: k /\ These a b
    -> Array (k /\ a) /\ Array (k /\ b)
    -> Array (k /\ a) /\ Array (k /\ b)
  f' (k /\ v) (as /\ bs) = case v of
    This a -> Tuple ((k /\ a) : as) bs
    That b -> Tuple as ((k /\ b) : bs)
    Both a b -> Tuple ((k /\ a) : as) ((k /\ b) : bs)

  mappedThese :: Array (k /\ a) /\ Array (k /\ b)
  mappedThese = foldr f' ([] /\ []) (unwrap $ map f mps)

-- | A singleton `Map`
singleton :: forall (k :: Type) (v :: Type). k -> v -> Map k v
singleton k v = Map $ Array.singleton (k /\ v)

-- | An empty `Map`
empty :: forall (k :: Type) (v :: Type). Map k v
empty = Map []

-- | Checks whether a `Map` is empty
null :: forall (k :: Type) (v :: Type). Map k v -> Boolean
null (Map xs) = Array.null xs

-- | Filter all values that satisfy the predicate.
filter :: forall (k :: Type) (v :: Type). (v -> Boolean) -> Map k v -> Map k v
filter f (Map xs) = Map $ Array.filter (f <<< snd) xs

-- | Return all elements of the map in the ascending order of their keys.
elems :: forall (k :: Type) (v :: Type). Map k v -> Array v
elems (Map xs) = snd <$> xs

-- | Map values and collect the `Just` results.
mapMaybe
  :: forall (k :: Type) (a :: Type) (b :: Type)
   . (a -> Maybe b)
  -> Map k a
  -> Map k b
mapMaybe f (Map xs) = Map $ Array.mapMaybe (\(k /\ v) -> (k /\ _) <$> f v) xs

-- | Map keys, values and collect the `Just` results.
mapMaybeWithKey
  :: forall (k :: Type) (a :: Type) (b :: Type)
   . (k -> a -> Maybe b)
  -> Map k a
  -> Map k b
mapMaybeWithKey f (Map xs) =
  Map $ Array.mapMaybe (\(k /\ v) -> (k /\ _) <$> f k v) xs
