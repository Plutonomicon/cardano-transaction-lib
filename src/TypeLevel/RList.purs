module TypeLevel.RList
  ( RList
  , Cons'
  , Nil'
  , class UniqueIndices
  , class AllUniqueLabels
  , class GetIndexWithLabel
  , class GetLabelWithIndex
  , class GetWithLabel
  ) where

import TypeLevel.Nat
import Type.RowList as RL
import Prim.TypeError (class Fail, Text)
import Data.Symbol

-- | A kind for UNORDERED RowLists which contain a Nat representation of their intended position in a
-- non-lexicographic order. Actual RowLists are automagically ordered lexicographically, Plutus Data is not ordered
-- lexicographically, so we need this to encode the structure of Plutus Data.
data RList k

-- | Type level "data constructors" for RList which only exist at the kind level. These are used to "pattern match" on
-- in type class instances so that we can perform various computations at the type level.
-- These correspond to RL.Cons/RL.Nil.
foreign import data Cons'
  :: forall (k :: Type) (nat :: Type). Symbol -> k -> nat -> RList k -> RList k

foreign import data Nil' :: forall (k :: Type). RList k

-- | Uniqueness constraint on the indices (i.e. the *Nat* indices) of a RList which asserts that all indices are unique.
-- This is needed to ensure that the various "lookup" classes return the "values" (types) that we expect.
class UniqueIndices :: forall (k :: Type). RList k -> Constraint
class UniqueIndices list

instance
  Fail (Text "Indices are not unique!") =>
  UniqueIndices (Cons' k a n (Cons' k' a' n xs))
else instance UniqueIndices Nil'
else instance UniqueIndices (Cons' k a n Nil')
else instance
  ( UniqueIndices (Cons' k a n xs)
  , UniqueIndices (Cons' k' a' n' xs)
  ) =>
  UniqueIndices (Cons' k a n (Cons' k' a' n' xs))

-- | Uniqueness constraint on the labels of a RList which asserts that all labels (Symbol indices) are unique.
-- Again, this is needed so that the lookup functions perform in the expected manner.
class AllUniqueLabels :: forall (k :: Type). RList k -> Constraint
class AllUniqueLabels list

instance
  Fail (Text "Labels are not unique!") =>
  AllUniqueLabels (Cons' k a n (Cons' k a' n' xs))
else instance AllUniqueLabels Nil'
else instance AllUniqueLabels (Cons' k a n Nil')
else instance
  ( AllUniqueLabels (Cons' k a n xs)
  , AllUniqueLabels (Cons' k' a' n' xs)
  ) =>
  AllUniqueLabels (Cons' k a n (Cons' k' a' n' xs))

-- | Get the Nat index corresponding to a given symbol index in an RList.
-- This is meant to be used primarily in the context of instances of other classes (see examples in ToData.purs),
-- and is probably not particularly useful outside of those instance contexts.
class GetIndexWithLabel
  :: forall (k :: Type) (n :: Type). Symbol -> RList k -> n -> Constraint
class IsSymbol label <= GetIndexWithLabel label list n | label list -> n

instance
  ( AllUniqueLabels (Cons' label a n xs)
  , UniqueIndices (Cons' label a n xs)
  , IsSymbol label
  , KnownNat n
  ) =>
  GetIndexWithLabel label (Cons' label a n xs) n

else instance
  ( GetIndexWithLabel label xs n
  , AllUniqueLabels (Cons' label' a' n' xs)
  , UniqueIndices (Cons' label' a' n' xs)
  , IsSymbol label
  , KnownNat n
  ) =>
  GetIndexWithLabel label (Cons' label' a' n' xs) n

-- | Given a Nat which appears as an index in the given RList, get ahold of its corresponding Symbol label
class GetLabelWithIndex
  :: forall (k :: Type) (n :: Type). n -> RList k -> Symbol -> Constraint
class (KnownNat ix) <= GetLabelWithIndex ix list label | ix list -> label

instance
  ( AllUniqueLabels (Cons' label a ix xs)
  , UniqueIndices (Cons' label a ix xs)
  , IsSymbol label
  , KnownNat ix
  ) =>
  GetLabelWithIndex ix (Cons' label a ix xs) label
else instance
  ( GetLabelWithIndex ix xs label
  , AllUniqueLabels (Cons' label' a' n' xs)
  , UniqueIndices (Cons' label' a' n' xs)
  , IsSymbol label
  , KnownNat ix
  ) =>
  GetLabelWithIndex ix (Cons' label' a' n' xs) label

--else instance (Fail (Text "Index does not exist in RList!"), KnownNat n) => GetLabelWithIndex n Nil' l

-- | Given a Symbol which appears as a label in the given RList, get ahold of the type indexed by that symbol.
-- Note that this does not return a *value* of that type, but the type itself, and is therefore only suitable
-- for type class instance contexts
class GetWithLabel :: forall (k :: Type). Symbol -> RList k -> k -> Constraint
class GetWithLabel label rlist result | label rlist -> result

instance IsSymbol l => GetWithLabel l (Cons' l a n xs) a
else instance (GetWithLabel l xs a) => GetWithLabel l (Cons' _l _a _n xs) a
