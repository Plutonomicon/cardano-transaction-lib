module TypeLevel.RowList
  ( class AllUniqueLabels
  , tests
  ) where

import Prim.TypeError (class Fail, Text)
import Type.RowList (Cons, Nil, RowList)

-- | Uniqueness constraint on the labels of an unordered RowList
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

-- | Poor man's type level tests
tests ∷ Array String
tests =
  [ testNil
  , testSingleton
  , testUniques
  -- , testDupsUnordered
  -- , testDups
  ]
  where
  testNil :: AllUniqueLabels Nil => String
  testNil = "Empty list has all unique labels"

  testSingleton
    :: forall (a :: Type). AllUniqueLabels (Cons "A" a Nil) => String
  testSingleton = "Singleton list has all unique labels"

  testUniques
    :: forall (a :: Type)
     . AllUniqueLabels
         ( Cons "A" a
             (Cons "B" a (Cons "C" a Nil))
         )
    => String
  testUniques = "[A, B, C] is all unique and should compile"

  _testDupsUnordered
    :: forall (a :: Type)
     . AllUniqueLabels (Cons "A" a (Cons "B" a (Cons "A" a (Cons "B" a Nil))))
    => String
  _testDupsUnordered = "[A, B, A, B] has duplicates but should compile"

  _testDups
    :: forall (a :: Type)
     . AllUniqueLabels (Cons "A" a (Cons "A" a Nil))
    => String
  _testDups = "[A, A] has duplicates and shouldn't compile"

