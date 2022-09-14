module CTL.Internal.TypeLevel.RowList
  ( class AllUniqueLabels
  ) where

import Prim.TypeError (class Fail, Text)
import Type.RowList (Cons, Nil, RowList)

-- | Uniqueness constraint on the labels of an unordered RowList
-- | Current implementation causes a compilation slowdown for complex types
-- | GH Issue: https://github.com/Plutonomicon/cardano-transaction-lib/issues/433
class AllUniqueLabels :: forall (k :: Type). RowList k -> Constraint
class AllUniqueLabels list

instance AllUniqueLabels Nil
instance AllUniqueLabels (Cons k a Nil)
instance
  Fail (Text "Labels are not unique!") =>
  AllUniqueLabels (Cons k a (Cons k a' xs))
else instance
  ( AllUniqueLabels (Cons k a xs)
  , AllUniqueLabels (Cons k' a' xs)
  ) =>
  AllUniqueLabels (Cons k a (Cons k' a' xs))
