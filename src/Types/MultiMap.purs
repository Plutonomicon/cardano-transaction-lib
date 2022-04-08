module Types.MultiMap
  ( MultiMap
  , empty
  , singleton
  , insert
  , delete
  , lookup
  ) where

import Control.Alt (map)
import Control.Bind ((>>=))
import Control.Category ((>>>))
import Data.Array as Array
import Data.Bounded (class Ord)
import Data.Function (($))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid ((<>))

-- | A specific-use MultiMap where values for the same key are stored in FIFO container
--   implemented via Array
newtype MultiMap (k :: Type) (v :: Type) = MultiMap (Map.Map k (Array v))

-- | Constructs an empty `MultiMap`
empty :: forall (k :: Type) (v :: Type). MultiMap k v
empty = MultiMap Map.empty

-- | Constructs a `MultiMap` storing single key-value pair.
singleton :: forall (k :: Type) (v :: Type). Ord k => k -> v -> MultiMap k v
singleton k v = MultiMap $ Map.insert k (Array.singleton v) Map.empty

-- | Inserts value. In case k exist the value is stored at the end of
--   existing values array.
insert
  :: forall (k :: Type) (v :: Type)
   . Ord k
  => k
  -> v
  -> MultiMap k v
  -> MultiMap k v
insert k v (MultiMap m) = MultiMap $ Map.insertWith (<>) k (Array.singleton v) m

-- | Deletes first value from a value array under given k.
delete
  :: forall (k :: Type) (v :: Type). Ord k => k -> MultiMap k v -> MultiMap k v
delete k (MultiMap m) = MultiMap $ Map.update (Array.uncons >>> map _.tail) k m

-- | Lookup first value for given key.
lookup :: forall (k :: Type) (v :: Type). Ord k => k -> MultiMap k v -> Maybe v
lookup k (MultiMap m) = Map.lookup k m >>= Array.head
