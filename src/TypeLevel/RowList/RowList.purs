module TypeLevel.RowList.Unordered where

import TypeLevel.Nat
import Type.RowList
import Prim.TypeError (class Fail, Text)
import Data.Symbol

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
