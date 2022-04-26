-- | `ConstrIndices` is a module that contains the `HasConstrIndices` type class which
-- | maps constructors in a Purescript sum type to a numeric index used in PlutusData
-- | `Constr` representation.
-- |
-- | For example, consider ToData/FromData instances (ie. PlutusData
-- | representation) for the Maybe type.
-- |
-- | ```purescript
-- | instance ToData a => ToData (Maybe a) where
-- |  toData (Just x) = Constr zero [ toData x ] -- Just is zero-indexed by Plutus
-- |  toData Nothing = Constr one []
-- |
-- | instance FromData a => FromData (Maybe a) where
-- |  fromData (Constr n [ pd ])
-- |    | n == zero = maybe Nothing (Just <<< Just) (fromData pd) -- Just is zero-indexed by Plutus
-- |  fromData (Constr n [])
-- |    | n == one = Just Nothing
-- |  fromData _ = Nothing
-- | ````
-- |
-- | Notice how the indices have to match exactly, to assure correct translation
-- | between PlutusData and Purescript types.
-- |
-- | Instead we could leverage the HasConstrIndices type class in the following way:
-- |
-- | ```purescript
-- | instance HasConstrIndices (Maybe a) where
-- |   constrIndices _ = fromConstr2Index [ Tuple "Just" 0, Tuple "Nothing" 1 ]
-- |
-- | instance (ToData a) => ToData (Maybe a) where
-- |   toData = genericToData
-- |
-- | instance (FromData a) => FromData (Maybe a) where
-- |   fromData = genericFromData
-- | ```
-- |
-- | This way we make sure the encoding and decoding is consistent.
-- |
-- | Another reason this is especially important is that Haskell and Purescript types used to
-- | write and interact with Plutus programs need to have compatible PlutusData representations.
-- | In Haskell, users are allowed to explicitly specify the constructor indices
-- | for Haskell sum types using
-- | [makeIsDataIndexed](https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-tx/src/PlutusTx/IsData/TH.hs#L133).
-- | That means that we cannot rely on any specific order and we must retain the
-- | ability in Purescript to specify the same constructor indices.
-- |
-- | However, if Haskell users used
-- | [unstableMakeIsData](https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-tx/src/PlutusTx/IsData/TH.hs#L129)
-- | which assumes 'default' constructor ordering, we also provide the same by using
-- | the `defaultConstrIndices`:
-- |
-- | ```purescript
-- | instance HasConstrIndices Foo where
-- |   constrIndices = defaultConstrIndices
-- | ```
module ConstrIndices
  ( class HasConstrIndices
  , class HasCountedConstrIndices
--  , Foo(..)
  , constrIndices
  , countedConstrIndices
  , defaultConstrIndices
  , fromConstr2Index
  ) where

import Prelude hiding (Ordering(..))
import Data.Show.Generic
import Data.Generic.Rep (class Generic)
import Data.Semigroup
import Prim.Ordering
import Data.Generic.Rep as G
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (reflectSymbol, SProxy(SProxy), class IsSymbol)
import Data.Tuple (Tuple(Tuple), swap)
import Type.Proxy (Proxy(Proxy))
import Type.RowList as RL
import Type.Row as R
import Type.Data.Ordering as Ord
import Prim.TypeError (class Fail, Text, Beside)

class HasConstrIndices :: Type -> Constraint
class HasConstrIndices a where
  constrIndices :: Proxy a -> Tuple (Map String Int) (Map Int String)

-- Default constructor indices
class HasCountedConstrIndices :: Type -> Constraint
class HasCountedConstrIndices a where
  countedConstrIndices
    :: Proxy a
    -> Int
    -> Tuple (Map String Int) (Map Int String)
    -> Tuple (Map String Int) (Map Int String)

instance
  ( HasCountedConstrIndices a
  , HasCountedConstrIndices b
  ) =>
  HasCountedConstrIndices (G.Sum a b) where
  countedConstrIndices _ i constrIx = countedConstrIndices (Proxy :: Proxy b)
    (i + 1)
    (countedConstrIndices (Proxy :: Proxy a) i constrIx)

instance (IsSymbol n) => HasCountedConstrIndices (G.Constructor n a) where
  countedConstrIndices _ i (Tuple constr2Ix ix2Constr) = Tuple
    (Map.insert (reflectSymbol (SProxy :: SProxy n)) i constr2Ix)
    (Map.insert i (reflectSymbol (SProxy :: SProxy n)) ix2Constr)

defaultConstrIndices
  :: forall (a :: Type) (rep :: Type)
   . G.Generic a rep
  => HasCountedConstrIndices rep
  => Proxy a
  -> Tuple (Map String Int) (Map Int String)
defaultConstrIndices _ = countedConstrIndices (Proxy :: Proxy rep) 0
  (Tuple Map.empty Map.empty) -- TODO: Do this computation at type level instead?

fromConstr2Index
  :: Array (Tuple String Int) -> Tuple (Map String Int) (Map Int String)
fromConstr2Index c2Is = Tuple
  (Map.fromFoldable c2Is)
  (Map.fromFoldable $ swap <$> c2Is)

{-
class IsPlutusData :: forall (k :: Type). Type -> RList (RList k) -> Constraint
class IsSorted2 list <= IsPlutusData t list








data Foo
  = F0
      { f0A :: String
      }
  | F1
      { f1A :: String
      , f1B :: String
      , f1C :: String
      }
  | F2
      { f2A :: String
      , f2B :: Boolean
      }

derive instance Generic Foo _

instance Show Foo where
  show x = genericShow x

instance HasConstrIndices Foo where
  constrIndices _ = fromConstr2Index [Tuple "F0" 0, Tuple "F1" 1, Tuple "F2" 2]

instance IndexedRecField Foo "f0A" Z

instance IndexedRecField Foo "f1A" (S Z)

instance IndexedRecField Foo "f1B" Z

instance IndexedRecField Foo "f1C" (S (S Z))

instance IndexedRecField Foo "f2A" Z

instance IndexedRecField Foo "f2B" (S Z)
-}
