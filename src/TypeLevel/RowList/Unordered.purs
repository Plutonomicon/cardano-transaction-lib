module TypeLevel.RowList.Unordered where

import TypeLevel.Nat
import Type.RowList as RL
import Prim.TypeError (class Fail, Text)
import Data.Symbol

-- | A kind for UNORDERED RowLists. Actual RowLists are automagically ordered lexicographically, Plutus Data is not ordered
-- lexicographically, so we need this to encode the structure of Plutus Data.
data RowList :: forall (k :: Type). k -> Type
data RowList k

-- | Type level "data constructors" for RList which only exist at the kind level. These are used to "pattern match" on
-- in type class instances so that we can perform various computations at the type level.
-- These (shockingly) correspond to RL.Cons/RL.Nil.
foreign import data Cons
  :: forall (k :: Type). Symbol -> k ->  RowList k -> RowList k

foreign import data Nil :: forall (k :: Type). RowList k

-- | Uniqueness constraint on the labels of an unordered RowList
class AllUniqueLabels :: forall (k :: Type). RowList k -> Constraint
class AllUniqueLabels list

instance
  Fail (Text "Labels are not unique!") =>
  AllUniqueLabels (Cons k a (Cons k a'  xs))
else instance AllUniqueLabels Nil
else instance AllUniqueLabels (Cons k a Nil)
else instance
  ( AllUniqueLabels (Cons k a xs)
  , AllUniqueLabels (Cons k' a' xs)
  ) =>
  AllUniqueLabels (Cons k a (Cons k' a' xs))
