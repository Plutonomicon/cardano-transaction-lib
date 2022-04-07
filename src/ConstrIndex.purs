module ConstrIndex
  ( class HasConstrIndex
  , class HasCountedConstrIndex
  , constrIndex
  , countedConstrIndex
  , defaultConstrIndex
  , fromConstr2Index
  ) where

import Type.Proxy (Proxy(Proxy))
import Data.Map (Map)
import Data.Map as Map
import Data.Generic.Rep as G
import Data.Symbol (reflectSymbol, SProxy(SProxy), class IsSymbol)
import Data.Tuple (Tuple(Tuple))
import Prelude

class HasConstrIndex (a :: Type) where
  constrIndex :: Proxy a -> Tuple (Map String Int) (Map Int String)

-- Default constructor indices
class HasCountedConstrIndex (a :: Type) where
  countedConstrIndex
    :: Proxy a
    -> Int
    -> Tuple (Map String Int) (Map Int String)
    -> Tuple (Map String Int) (Map Int String)

instance
  ( HasCountedConstrIndex a
  , HasCountedConstrIndex b
  ) =>
  HasCountedConstrIndex (G.Sum a b) where
  countedConstrIndex _ i constrIx = countedConstrIndex (Proxy :: Proxy b)
    (i + 1)
    (countedConstrIndex (Proxy :: Proxy a) i constrIx)

instance (IsSymbol n) => HasCountedConstrIndex (G.Constructor n a) where
  countedConstrIndex _ i (Tuple constr2Ix ix2Constr) = Tuple
    (Map.insert (reflectSymbol (SProxy :: SProxy n)) i constr2Ix)
    (Map.insert i (reflectSymbol (SProxy :: SProxy n)) ix2Constr)

defaultConstrIndex
  :: forall a rep
   . G.Generic a rep
  => HasCountedConstrIndex rep
  => Proxy a
  -> Tuple (Map String Int) (Map Int String)
defaultConstrIndex _ = countedConstrIndex (Proxy :: Proxy rep) 0
  (Tuple Map.empty Map.empty) -- TODO: Do this computation at type level instead?

fromConstr2Index
  :: Array (Tuple String Int) -> Tuple (Map String Int) (Map Int String)
fromConstr2Index c2Is = Tuple
  (Map.fromFoldable c2Is)
  (Map.fromFoldable $ (\(Tuple c i) -> Tuple i c) <$> c2Is)
