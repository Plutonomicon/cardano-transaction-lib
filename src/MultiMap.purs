module MultiMap
  ( MultiMap
  , empty
  , singleton
  , insert
  , delete
  , lookup
  )
  where

import Control.Alt (map)
import Control.Bind ((>>=))
import Control.Category ((>>>))
import Data.Array as Array
import Data.Bounded (class Ord)
import Data.Function (($))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid ((<>))

newtype MultiMap k v = MultiMap (Map.Map k (Array v))

empty :: forall k v. MultiMap k v
empty = MultiMap Map.empty

singleton :: forall k v. Ord k => k -> v -> MultiMap k v
singleton k v = MultiMap $ Map.insert k (Array.singleton v) Map.empty

insert :: forall k v. Ord k => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap m)= MultiMap $ Map.insertWith (<>) k (Array.singleton v) m

delete :: forall k v. Ord k => k -> MultiMap k v -> MultiMap k v
delete k (MultiMap m)= MultiMap $ Map.update (Array.uncons >>> map _.tail) k m

lookup :: forall k v. Ord k => k -> MultiMap k v -> Maybe v
lookup k (MultiMap m) = Map.lookup k m >>= Array.head
