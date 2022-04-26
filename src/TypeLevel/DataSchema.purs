module TypeLevel.DataSchema where

import TypeLevel.RList
import TypeLevel.Nat
import Data.Symbol
import Type.Proxy
import Data.Unit


class OrderedRecIndices :: forall (k :: Type). RList (RList k) -> Constraint
class OrderedRecIndices rlist

instance OrderedRecIndices Nil'
else instance (StrictlyIncreasing a, OrderedRecIndices xs) => OrderedRecIndices (Cons' k a n xs)


-- We don't want to accept malformed record field indices
class StrictlyIncreasing :: forall (k :: Type). RList k -> Constraint
class StrictlyIncreasing rList

instance StrictlyIncreasing Nil'
else instance StrictlyIncreasing (Cons' k a n Nil')
else instance (StrictlyIncreasing xs, StrictlyIncreasing (Cons' k' a' (S n) xs))
              => StrictlyIncreasing (Cons' k a n (Cons' k' a' (S n)  xs))

class AllUnique2 :: forall (k :: Type). RList (RList k) -> Constraint
class AllUnique2 rList

instance AllUnique2 Nil'
else instance (AllUniqueLabels a,
               UniqueIndices a,
               AllUniqueLabels (Cons' l a n xs),
               UniqueIndices (Cons' l a n xs),
               AllUnique2 xs
               ) => AllUnique2 (Cons' l a n xs)

class ValidPlutusSchema :: forall (k :: Type). PlutusSchema k -> RList (RList k) -> Constraint
class (PlutusSchemaToRList schema list, AllUnique2 list, OrderedRecIndices list) <= ValidPlutusSchema schema list | schema -> list
instance (PlutusSchemaToRList schema list, AllUnique2 list, OrderedRecIndices list) => ValidPlutusSchema schema list


class HasPlutusSchema :: forall (k :: Type). Type -> PlutusSchema k -> Constraint
class HasPlutusSchema t schema  | t -> schema

type PlutusSchema k = PSchema (PSchema k)

data PSchema k

data IxK k n

foreign import data MkIxK :: forall (k :: Type) (n :: Type). k -> n -> IxK k n

type MkIxK_ k n = MkIxK k n

infixr 9 type MkIxK_ as @@

data Field k

foreign import data MkField  :: forall  (k :: Type) (n :: Type). Symbol  -> IxK k n ->  Field k

type MkField_ lbl ixty = MkField lbl ixty

infixr 8 type MkField_ as :=

foreign import data PNil  :: forall (k :: Type). PSchema k
foreign import data PCons :: forall (k :: Type). Field k -> PSchema k -> PSchema k

type ApPCons field schema = PCons field schema

infixr 0 type ApPCons as :+

class SchemaToRList :: forall (k :: Type). PSchema k -> RList k -> Constraint
class SchemaToRList schema list | schema -> list

instance SchemaToRList PNil Nil'
else instance (SchemaToRList xs xs', KnownNat n, IsSymbol l) => SchemaToRList (PCons (MkField l (MkIxK k n)) xs) (Cons' l k n xs')

class PlutusSchemaToRList :: forall (k :: Type). PlutusSchema k -> RList (RList k) -> Constraint
class PlutusSchemaToRList schema list | schema -> list

instance PlutusSchemaToRList PNil Nil'
else instance (PlutusSchemaToRList xs xs', SchemaToRList a a', KnownNat n, IsSymbol l) => PlutusSchemaToRList (PCons (MkField l (MkIxK a n)) xs) (Cons' l a' n xs')


data Foo
  = F0
      { f0A :: String
      }
  | F1
      { f1A :: String
      , f1B :: String
      , f1C :: String
      }
  | F2
      { f2A :: String
      , f2B :: Boolean
      }

instance HasPlutusSchema Foo (
           "F0" := ("f0A" := String @@ Z :+ PNil) @@ Z

        :+ "F1" := (   "f1A" := String @@ Z
                    :+ "f1B" := String @@ (S Z)
                    :+ "f1C" := String @@ (S (S Z))
                    :+ PNil)
                @@ (S Z)

        :+ "F2" :=  (  "f2A" := String @@ Z
                    :+ "f2B" := Boolean @@ (S Z)
                    :+ PNil)
                @@ (S (S Z))

        :+ PNil)

