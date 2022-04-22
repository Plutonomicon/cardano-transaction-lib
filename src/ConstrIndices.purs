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
  , class IndexedRecFieldT
  , RList
  , Cons'
  , Nil'
  , S
  , Z
  , class SortRec
  , class ToRList
  , class Split
  , class Merge
  , class Sort
  , class FromRList
  , class RListToRow
  , class KnownNat
  , class RowToRList
  , natVal
  , constrIndices
  , countedConstrIndices
  , getFieldIndex
  , defaultConstrIndices
  , fromConstr2Index
  ) where

import Prelude hiding (Ordering(..))

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

class (IsSymbol s, KnownNat n) <= IndexedRecFieldT (t :: Type) s n | t s -> n

class KnownNat n where
  natVal :: Proxy n -> Int

instance KnownNat Z where
  natVal _ = 0

instance KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

-- a kind for UNORDERED rowlists. bleh
data RList k

foreign import data Cons' :: forall (k :: Type) (nat :: Type). Symbol -> k -> nat ->  RList k -> RList k
foreign import data Nil'  :: forall (k :: Type). RList k

class SortRec :: forall k. Type -> RL.RowList k -> RList k -> Constraint
class SortRec t rowList rList | t rowList -> rList
instance (ToRList t rowList rList,  Sort t rList' rList) => SortRec t rowList rList

class ToRList :: forall (k :: Type). Type -> RL.RowList k -> RList k -> Constraint
class ToRList t rowList rList | t rowList -> rList

instance ToRList t RL.Nil Nil'
--else instance (IndexedRecFieldT t key Z) => ToRList t (RL.Cons key a RL.Nil) (Cons' key a Z Nil')
instance (ToRList t as as', IndexedRecFieldT t key n) => ToRList t (RL.Cons key a as) (Cons' key a n as')



class Sort :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class Sort t rList result | t rList -> result

instance Sort t Nil' Nil'
else instance Sort t (Cons' key a nA Nil') (Cons' key a nA Nil')
else instance (Split xs ls rs, Sort t ls ls', Sort t rs rs', Merge t ls' rs' merged)
              =>  Sort t xs merged

class Split :: forall (k :: Type). RList k ->  (RList k) -> (RList k) -> Constraint
class Split k resultL resultR | k -> resultL, k -> resultR

instance Split  Nil'  Nil' Nil'
else instance Split (Cons' key a nA Nil')  (Cons' key a nA Nil') Nil'
else instance Split rest as bs => Split (Cons' keyA a nA (Cons' keyB b nB rest)) (Cons' keyA a nA as) (Cons' keyB b nB bs)

class Merge :: forall (k :: Type). Type -> (RList k) ->  (RList k) -> RList k -> Constraint
class Merge t listL listR result | listL listR -> result

instance Merge t Nil' Nil' Nil'
else instance Merge t (Cons' keyA a nA as)  Nil' (Cons' keyA a nA as)

else instance Merge t Nil' (Cons' keyB b nB bs) (Cons' keyB b nB bs)

else instance Merge t as (Cons' keyB b Z  bs) merged
                => Merge t (Cons' keyA a Z as) (Cons' keyB b Z  bs) (Cons' keyA a Z merged)

else instance Merge t as (Cons' keyB b Z  bs) merged
                => Merge t (Cons' keyA a (S nA) as) (Cons' keyB b Z  bs) (Cons' keyA a (S nA)  merged)

else instance Merge t (Cons' keyA a Z as) bs merged
                => Merge t (Cons' keyA a Z as) (Cons' keyB b (S nB)  bs) (Cons' keyB b (S nB)  merged)

else instance (Merge t (Cons' keyA a nA as) (Cons' keyB b nB bs) merged)
                => Merge t (Cons' keyA a (S nA) as) (Cons' keyB b (S nB) bs) merged

class FromRList :: forall (k :: Type). RList k -> RL.RowList k -> Constraint
class FromRList rList rowList | rList -> rowList

instance FromRList Nil' RL.Nil
else instance FromRList rest result' => FromRList (Cons' key a n rest) (RL.Cons key a result)

class RListToRow :: forall (k :: Type). RList k -> Row k -> Constraint
class RListToRow rList row | rList -> row

instance (FromRList rList rowList, RL.ListToRow rowList row) => RListToRow rList row

class RowToRList :: forall (k :: Type). Type -> Row k -> RList k -> Constraint
class RowToRList t row rList | t row -> rList

instance (RL.RowToList row rowList, ToRList t rowList rList) => RowToRList t row rList

{-
class RListOf :: forall (k :: Type). RL.RowList k -> RList k -> Constraint
class (ToRList rowList rList, FromRList rList rowList) <= RListOf rowList rList | rowList -> rList, rList -> rowList
instance (ToRList rowList rList, FromRList rList rowList) => RListOf rowList rList

class Sort :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class Sort t rList result | t rList -> result
-}

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
