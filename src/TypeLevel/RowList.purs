module TypeLevel.RowList
  ( class AllUniqueLabels
  , tests
  )
  where

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
  [ _testNil
  , _testSingleton
  , _testUniques
  -- , _testDupsUnordered
  -- , _testDups -- Should trigger 'Labels are not unique!'
  ]
  where
  _testNil :: AllUniqueLabels Nil => String
  _testNil = "Empty list has all unique labels"

  _testSingleton
    :: forall (a :: Type). AllUniqueLabels (Cons "A" a Nil) => String
  _testSingleton = "Singleton list has all unique labels"

  _testUniques
    :: forall (a :: Type)
     . AllUniqueLabels
         ( Cons "A" a
             (Cons "B" a (Cons "C" a Nil))
         )
    => String
  _testUniques = "[A, B, C] is all unique and should compile"

  -- TODO: Explain why the type checker triggers 'Labels are not unique'
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

