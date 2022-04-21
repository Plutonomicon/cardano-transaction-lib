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
  , class IndexedRecField
  , constrIndices
  , countedConstrIndices
  , getFieldIndex
  , defaultConstrIndices
  , fromConstr2Index
  ) where

import Prelude

import Data.Generic.Rep as G
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (reflectSymbol, SProxy(SProxy), class IsSymbol)
import Data.Tuple (Tuple(Tuple), swap)
import Type.Proxy (Proxy(Proxy))
import Type.RowList as RL
import Type.Row as R
import Type.Data.Ordering as Ord
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

class (IsSymbol s) <= IndexedRecField (t :: Type) s where
  getFieldIndex :: Proxy t -> SProxy s -> Int

data Z

data S n

class (IsSymbol s, KnownNat n) <= IndexedRecFieldT (t :: Type) s n | t s -> n where
  getFieldIndexT :: Proxy t -> SProxy s -> Proxy n


class KnownNat n where
  natVal :: Proxy n -> Int

instance KnownNat Z where
  natVal _ = 0

instance KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

class (KnownNat n1, KnownNat n2, KnownNat n3) <= Maximum n1 n2 n3

instance (KnownNat s) => Maximum Z s s
else instance (KnownNat s) => Maximum s Z s
else instance (KnownNat n1, KnownNat n2, KnownNat n3, Maximum n1 n2 n3) => Maximum (S n1) (S n2) (S n3)

class KnownNat n <= MaxFieldIndex (t :: Type) rlist  n | t rlist -> n

instance (IndexedRecFieldT t s n) => MaxFieldIndex t (RL.Cons s x RL.Nil) n
else instance (MaxFieldIndex t xs n1, KnownNat n2, IsSymbol s, IndexedRecFieldT t s n2, Maximum n1 n2 n3) => MaxFieldIndex t (RL.Cons s x xs) n3

-- a kind for UNORDERED rowlists. bleh
data RList k

foreign import data Cons' :: forall (k :: Type). Symbol -> k -> RList k -> RList k
foreign import data Nil'  :: forall (k :: Type). RList k

class SortRec :: forall k. Type -> RL.RowList k -> RList k -> Constraint
class SortRec t rowList rList

class KnownNat n <= Length rowList n

instance Length RL.Nil Z
instance (Length xs n, KnownNat n) => Length (RL.Cons s a xs) (S n)

class ToRList :: forall (k :: Type). RL.RowList k -> RList k -> Constraint
class ToRList rowList rList

instance ToRList RL.Nil Nil'
else instance (ToRList xs xs') => ToRList (RL.Cons key a xs) (Cons' key a xs')

-- these can be made "safer"
class Take :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class Take n rList rList' | n rList -> rList'

instance Take Z rList Nil'
else instance Take n Nil' Nil'
else instance Take n xs ys => Take (S n) (Cons' key a xs) (Cons' key a ys)

class Drop :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class Drop n rList rList'

instance  Drop Z rList rList
else instance Drop n Nil' Nil'
else instance Drop n xs ys => Drop (S n) (Cons' key a xs) ys

class SplitAt :: forall (k :: Type). Type -> RList k -> Tuple (RList k) (RList k) -> Constraint
class (KnownNat n) <= SplitAt n rlist tup

instance (Take n rList taken, Drop n rList dropped) => SplitAt n rList (Tuple taken dropped)

{-
data SortedRowList n k

data Nil' z k

data Cons' s n k xs

class SortRec (t :: Type) (n :: Type) (list :: RowList k)  (sorted :: Type) | t list -> n, t list n -> sorted

instance SortRec t Nil Z (Nil' Z k)

instance
  ( SortRec t n listRest sortedListRest
  , RL.Cons key a rowRest rowFull
  , IsSymbol key
  , IndexedRecFieldT t key (S n)
  ) => SortRec t (S n) (RL.Cons key a rowRest rowFull)  (Cons' s )
-}
