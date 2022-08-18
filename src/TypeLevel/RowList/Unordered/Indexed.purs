module TypeLevel.RowList.Unordered.Indexed
  ( ConsI
  , NilI
  , RowListI
  , class AllUniqueLabelsI
  , class GetIndexWithLabel
  , class GetLabelIndex
  , class GetLabelWithIndex
  , class GetWithLabel
  , class IndexRowList
  , class IndexRowListWithAcc
  , class UniqueIndices
  ) where

import Data.Symbol

import Prim.TypeError (class Fail, Text)
import Type.RowList (Cons, Nil, RowList)
import TypeLevel.Nat (class KnownNat, Nat, S, Z)

-- | A kind for unordered  RowLists which contain a Nat representation of their
-- | intended position in a  non-lexicographic order. Actual RowLists are automagically
-- | ordered lexicographically, Haskell records are not ordered lexicographically, so we need
-- | this to encode the structure of Haskell records precisely.
data RowListI :: forall (k :: Type). k -> Type
data RowListI k

-- Data constructors for RowListI.
foreign import data ConsI
  :: forall (k :: Type). Symbol -> k -> Nat -> RowListI k -> RowListI k

foreign import data NilI :: forall (k :: Type). RowListI k

-- TODO: See if we can optimize this. Slows down compilation time considerably for complex types
-- GH Issue: https://github.com/Plutonomicon/cardano-transaction-lib/issues/433
-- | Uniqueness constraint on the Nat indices of a RowListI which asserts that all indices
-- | are unique. This is needed to ensure that the various "lookup" classes return the
-- | "values" (types) that we expect.
class UniqueIndices :: forall (k :: Type). RowListI k -> Constraint
class UniqueIndices list

instance UniqueIndices NilI
instance UniqueIndices (ConsI k a n NilI)
instance
  Fail (Text "Indices are not unique!") =>
  UniqueIndices (ConsI k a n (ConsI k' a' n xs))
else instance
  ( UniqueIndices (ConsI k a n xs)
  , UniqueIndices
      (ConsI k' a' n' xs)
  ) =>
  UniqueIndices (ConsI k a n (ConsI k' a' n' xs))

-- TODO: See if we can optimize this. Slows down compilation considerably for complex types.
-- GH Issue: https://github.com/Plutonomicon/cardano-transaction-lib/issues/433
-- | Uniqueness constraint on the labels of a RowListI which asserts that all labels are unique.
-- | Again, this is needed so that the lookup functions perform in the expected manner.
class AllUniqueLabelsI :: forall (k :: Type). RowListI k -> Constraint
class AllUniqueLabelsI list

instance AllUniqueLabelsI NilI
instance AllUniqueLabelsI (ConsI k a n NilI)
instance
  Fail (Text "Labels are not unique!") =>
  AllUniqueLabelsI (ConsI k a n (ConsI k a' n' xs))
else instance
  ( AllUniqueLabelsI (ConsI k a n xs)
  , AllUniqueLabelsI (ConsI k' a' n' xs)
  ) =>
  AllUniqueLabelsI (ConsI k a n (ConsI k' a' n' xs))

-- | Get the Nat index corresponding to a given symbol index in a RowListI.
-- | This is meant to be used primarily in the context of an instance declaration for some
-- | other class  (see examples in ToData.purs), and is probably not particularly useful
-- | outside of those instance contexts.
class GetIndexWithLabel
  :: forall (k :: Type). Symbol -> RowListI k -> Nat -> Constraint
class IsSymbol label <= GetIndexWithLabel label list n | label list -> n

instance
  ( AllUniqueLabelsI (ConsI label a n xs)
  , UniqueIndices (ConsI label a n xs)
  , IsSymbol label
  , KnownNat n
  ) =>
  GetIndexWithLabel label (ConsI label a n xs) n

else instance
  ( GetIndexWithLabel label xs n
  , AllUniqueLabelsI (ConsI label' a' n' xs)
  , UniqueIndices (ConsI label' a' n' xs)
  , IsSymbol label
  , KnownNat n
  ) =>
  GetIndexWithLabel label (ConsI label' a' n' xs) n

-- | Given a Nat which appears as an index in the given RowListI, get its corresponding Symbol label
class GetLabelWithIndex
  :: forall (k :: Type). Nat -> RowListI k -> Symbol -> Constraint
class
  ( KnownNat ix
  , AllUniqueLabelsI list
  , UniqueIndices list
  ) <=
  GetLabelWithIndex ix list label
  | ix list -> label

instance
  ( AllUniqueLabelsI (ConsI label a ix xs)
  , UniqueIndices (ConsI label a ix xs)
  , IsSymbol label
  , KnownNat ix
  ) =>
  GetLabelWithIndex ix (ConsI label a ix xs) label
else instance
  ( GetLabelWithIndex ix xs label
  , AllUniqueLabelsI (ConsI label' a' n' xs)
  , UniqueIndices (ConsI label' a' n' xs)
  , IsSymbol label
  , KnownNat ix
  ) =>
  GetLabelWithIndex ix (ConsI label' a' n' xs) label

-- | Given a Symbol which appears as a label in the given RowListI, get ahold of the type indexed by that symbol.
-- | Note that this does not "return" a *value* of that type, but the type itself, and is therefore only suitable
-- | for type class instance contexts (& other type level computational contexts if any exist)
class GetWithLabel
  :: forall (k :: Type). Symbol -> RowListI k -> k -> Constraint
class GetWithLabel label rlist result | label rlist -> result

instance IsSymbol l => GetWithLabel l (ConsI l a n xs) a
else instance (GetWithLabel l xs a) => GetWithLabel l (ConsI _l _a _n xs) a

-- | Get the nat index at a symbol in a RowList (Note: Not a RowListI! Use GetIndexWithLabel for a RowListI)
class GetLabelIndex
  :: forall (k :: Type). Symbol -> RowList k -> Nat -> Constraint
class GetLabelIndex label list result | label list -> result

instance
  ( IndexRowList rowList rowListI
  , GetIndexWithLabel label rowListI result
  ) =>
  GetLabelIndex label rowList result

-- | Helper class for GetLabelIndex. Converts a RowList into a RowListI using its natural order (beginning with Z)
class IndexRowList :: forall (k :: Type). RowList k -> RowListI k -> Constraint
class IndexRowList list result | list -> result

instance IndexRowListWithAcc Z list result => IndexRowList list result

-- | Helper class for IndexRowListWithAcc.
class IndexRowListWithAcc
  :: forall (k :: Type). Nat -> RowList k -> RowListI k -> Constraint
class IndexRowListWithAcc acc list result | acc list -> result

-- purescript sure is an ugly dialect of prolog
instance IndexRowListWithAcc n Nil NilI
else instance
  IndexRowListWithAcc (S n) xs xs' =>
  IndexRowListWithAcc n (Cons l a xs) (ConsI l a n xs')
