module TypeLevel.Sort (
  class Sort,
  class Split,
  class Merge,
  class RowToRList
  ) where

import TypeLevel.RList
import TypeLevel.Nat
import TypeLevel.IndexedRecField
import Type.RowList as RL

-- Sorts an RList using the IndexedRecFieldT instances of t.
-- This is basically mergesort but we use pattern matching instead of a compare function for the merge
-- NOTE: Sorts low -> high
class Sort :: forall (k :: Type). Type -> RList k -> RList k -> Constraint
class Sort t rList result | t rList -> result

instance Sort t Nil' Nil'
else instance Sort t (Cons' key a nA Nil') (Cons' key a nA Nil')
else instance (Split xs ls rs, Sort t ls ls', Sort t rs rs', Merge t ls' rs' merged)
              =>  Sort t xs merged

-- Divides a RList in half. We use a "Prolog style" split instead of Length/SplitAt for simplicity
class Split :: forall (k :: Type). RList k ->  (RList k) -> (RList k) -> Constraint
class Split k resultL resultR | k -> resultL, k -> resultR

instance Split  Nil'  Nil' Nil'
else instance Split (Cons' key a nA Nil')  (Cons' key a nA Nil') Nil'
else instance Split rest as bs => Split (Cons' keyA a nA (Cons' keyB b nB rest)) (Cons' keyA a nA as) (Cons' keyB b nB bs)

-- Combines two sorted RLists into one sorted RList (hopefully!)
class Merge :: forall (k :: Type). Type -> (RList k) ->  (RList k) -> RList k -> Constraint
class Merge t listL listR result | listL listR -> result

instance Merge t Nil' Nil' Nil'
else instance Merge t (Cons' keyA a nA as)  Nil' (Cons' keyA a nA as)

else instance Merge t Nil' (Cons' keyB b nB bs) (Cons' keyB b nB bs)

else instance (Merge t as (Cons' keyB b (S nB) bs) merged)
                => Merge t (Cons' keyA a Z as)
                           (Cons' keyB b (S nB) bs)
                           --------------------
                           (Cons' keyA a Z merged)

else instance ( Merge t (Cons' keyA a (S nA) as) bs  merged
              )  => Merge t (Cons' keyA a (S nA) as)
                            (Cons' keyB b Z bs)
                            -------------------
                            (Cons' keyB b Z merged)

else instance (Merge t (Cons' keyA a nA as) (Cons' keyB b nB bs) (Cons' keyX x nX xs))
                => Merge t (Cons' keyA a (S nA) as) (Cons' keyB b (S nB) bs) (Cons' keyX x (S nX) xs)

-- | Convert a Row Type into an RList using its "key type"
class RowToRList :: forall (k :: Type). Type -> Row k -> RList k -> Constraint
class RowToRList t row rList | t row -> rList

instance (RL.RowToList row rowList, ToRList t rowList rList) => RowToRList t row rList
