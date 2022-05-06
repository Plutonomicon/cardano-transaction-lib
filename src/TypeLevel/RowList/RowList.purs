module TypeLevel.RowList.Unordered
  ( class AllUniqueLabels
  ) where

import Type.RowList (Cons, Nil, RowList)
import Prim.TypeError (class Fail, Text)

-- | Uniqueness constraint on the labels of an unordered RowList
class AllUniqueLabels :: forall (k :: Type). RowList k -> Constraint
class AllUniqueLabels list

instance
  Fail (Text "Labels are not unique!") =>
  AllUniqueLabels (Cons k a (Cons k a' xs))
else instance AllUniqueLabels Nil
else instance AllUniqueLabels (Cons k a Nil)
else instance
  ( AllUniqueLabels (Cons k a xs)
  , AllUniqueLabels (Cons k' a' xs)
  ) =>
  AllUniqueLabels (Cons k a (Cons k' a' xs))
