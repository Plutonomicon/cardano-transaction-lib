module TypeLevel.RList where

import TypeLevel.Nat
import TypeLevel.IndexedRecField
import Type.RowList as RL
import Prim.TypeError (class Fail, Text)
import Data.Symbol
-- a kind for UNORDERED RowLists which contain a Nat representation of their intended position in a
-- non-lexicographic order. Actual RowLists are automagically ordered lexicographically so we need this
data RList k

foreign import data Cons' :: forall (k :: Type) (nat :: Type). Symbol -> k -> nat ->  RList k -> RList k
foreign import data Nil'  :: forall (k :: Type). RList k

-- | We convert from a RowList to a RList using the IndexedRecFieldT instances of t
class ToRList :: forall (k :: Type). Type -> RL.RowList k -> RList k -> Constraint
class ToRList t rowList rList | t rowList -> rList

instance ToRList t RL.Nil Nil'
instance (ToRList t as as', IndexedRecField t key n) => ToRList t (RL.Cons key a as) (Cons' key a n as')

class FromRList :: forall (k :: Type). RList k -> RL.RowList k -> Constraint
class FromRList rList rowList | rList -> rowList, rowList -> rList

instance FromRList Nil' RL.Nil
else instance (FromRList as' as) => FromRList (Cons' key a n as') (RL.Cons key a as)

class UniqueIndices :: forall (k :: Type). RList k -> Constraint
class UniqueIndices list

instance Fail (Text "Indices are not unique!") => UniqueIndices (Cons' k a n (Cons' k' a' n xs))
else instance UniqueIndices Nil'
else instance UniqueIndices (Cons' k a n Nil')
else instance (UniqueIndices (Cons' k a n xs), UniqueIndices (Cons' k' a' n' xs)) => UniqueIndices (Cons' k a n (Cons' k' a' n' xs))


class AllUniqueLabels :: forall (k :: Type). RList k -> Constraint
class AllUniqueLabels list

instance Fail (Text "Labels are not unique!") => AllUniqueLabels (Cons' k a n (Cons' k a' n' xs))
else instance AllUniqueLabels Nil'
else instance AllUniqueLabels (Cons' k a n Nil')
else instance (AllUniqueLabels (Cons' k a n xs), AllUniqueLabels (Cons' k' a' n' xs)) => AllUniqueLabels (Cons' k a n (Cons' k' a' n' xs))

-- Unfortunately our type level mergesort alters the nat index of the elements of its RList  (I can't think of a way around that without backtracking)
-- consequently, we have to relabel the fields with their proper indices in order to do "reverse lookups" of symbol from their nats
class RelabelRecFields :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class RelabelRecFields t list result | t list -> result

instance RelabelRecFields t Nil' Nil'
else instance (IndexedRecField t label n, RelabelRecFields t _xs xs)
              => RelabelRecFields
                   t
                   (Cons' label a _n _xs)
                   --------------------
                   (Cons' label a n xs)


-- "Normal" lookup
class GetIndexWithLabel :: forall (k :: Type) (n :: Type). Symbol -> RList k -> n -> Constraint
class IsSymbol label <=  GetIndexWithLabel label list n | label list -> n



instance (AllUniqueLabels (Cons' label a n xs),
               UniqueIndices (Cons' label a n xs),
               IsSymbol label,
               KnownNat n) => GetIndexWithLabel label (Cons' label a n xs) n

else instance (GetIndexWithLabel label xs n,
               AllUniqueLabels (Cons' label' a' n' xs),
               UniqueIndices (Cons' label' a' n' xs),
               IsSymbol label,
               KnownNat n) => GetIndexWithLabel label (Cons' label' a' n' xs) n

--else instance (Fail (Text "Label does not exist in RList!"), IsSymbol label) => GetIndexWithLabel label Nil' a

-- Reverse lookup
class GetLabelWithIndex :: forall (k :: Type) (n :: Type). n -> RList k -> Symbol -> Constraint
class (KnownNat ix) <= GetLabelWithIndex ix list label | ix list -> label


instance (AllUniqueLabels (Cons' label a ix xs),
               UniqueIndices (Cons' label a ix xs),
               IsSymbol label,
               KnownNat ix) => GetLabelWithIndex ix (Cons' label a ix xs) label
else instance (GetLabelWithIndex ix xs label,
               AllUniqueLabels (Cons' label' a' n' xs),
               UniqueIndices (Cons' label' a' n' xs),
               IsSymbol label,
               KnownNat ix) => GetLabelWithIndex ix (Cons' label' a' n' xs) label
--else instance (Fail (Text "Index does not exist in RList!"), KnownNat n) => GetLabelWithIndex n Nil' l


class GetWithLabel :: forall (k :: Type). Symbol -> RList k -> k -> Constraint
class  GetWithLabel label rlist result | label rlist -> result

instance IsSymbol l => GetWithLabel l (Cons' l a n xs) a
else instance (GetWithLabel l xs a) => GetWithLabel l (Cons' _l _a _n xs) a
