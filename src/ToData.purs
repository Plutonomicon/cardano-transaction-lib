module ToData
  ( AnotherDay(..)
  , Day(..)
  , Tree(..)
  , class ConstrIndex
  , class CountedConstrIndex
  , class ToData
  , class ToDataArgs
  , class ToDataWithIndex
  , constrIndex
  , countedConstrIndex
  , defaultConstrIndex
  , genericToData
  , toData
  , toDataArgs
  , toDataWithIndex
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((..))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Foldable (class Foldable)
import Data.Generic.Rep as G
import Data.Map (Map)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Profunctor.Strong ((***))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Helpers (uIntToBigInt)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Constr, Integer, List, Map, Bytes))

-- Classes

class ToData (a :: Type) where
  toData :: a -> PlutusData

class ConstrIndex (a :: Type) where
  constrIndex :: Proxy a -> Map String Int

class ToDataWithIndex a ci where
  toDataWithIndex :: ConstrIndex ci => Proxy ci -> a -> PlutusData

-- As explained in https://harry.garrood.me/blog/write-your-own-generics/ this
-- is just a neat pattern that flattens a skewed Product of Products
class ToDataArgs a where
  toDataArgs :: a -> Array (PlutusData)

-- Default constructor indices
class CountedConstrIndex (a :: Type) where
  countedConstrIndex :: Proxy a -> Int -> Map String Int -> Map String Int

-- Data.Generic.Rep instances

instance toDataWithIndexSum ::
  ( ConstrIndex a
  , ToDataWithIndex l a
  , ToDataWithIndex r a
  ) =>
  ToDataWithIndex (G.Sum l r) a where
  toDataWithIndex p (G.Inl x) = toDataWithIndex (p :: Proxy a) x
  toDataWithIndex p (G.Inr x) = toDataWithIndex (p :: Proxy a) x

instance toDataWithIndexConstr ::
  ( IsSymbol n
  , ConstrIndex a
  , ToDataArgs arg
  ) =>
  ToDataWithIndex (G.Constructor n arg) a where
  toDataWithIndex p (G.Constructor args) = Constr (resolveIndex p (SProxy :: SProxy n)) (toDataArgs args)

instance toDataArgsNoArguments :: ToDataArgs G.NoArguments where
  toDataArgs _ = []

instance toDataArgsArgument :: ToData a => ToDataArgs (G.Argument a) where
  toDataArgs (G.Argument x) = [ toData x ]

instance toDataArgsProduct :: (ToDataArgs a, ToDataArgs b) => ToDataArgs (G.Product a b) where
  toDataArgs (G.Product x y) = toDataArgs x <> toDataArgs y

instance (CountedConstrIndex a, CountedConstrIndex b) => CountedConstrIndex (G.Sum a b) where
  countedConstrIndex _ i sym2ix = countedConstrIndex (Proxy :: Proxy b) (i + 1) (countedConstrIndex (Proxy :: Proxy a) i sym2ix)

instance (IsSymbol n) => CountedConstrIndex (G.Constructor n a) where
  countedConstrIndex _ i sym2ix = (Map.insert (reflectSymbol (SProxy :: SProxy n)) i sym2ix)

genericToData
  :: forall a rep. G.Generic a rep => ConstrIndex a => ToDataWithIndex rep a => a -> PlutusData
genericToData = toDataWithIndex (Proxy :: Proxy a) <<< G.from

defaultConstrIndex :: forall a rep. G.Generic a rep => CountedConstrIndex rep => Proxy a -> Map String Int
defaultConstrIndex _ = countedConstrIndex (Proxy :: Proxy rep) 0 Map.empty -- TODO: Do this computation at type level instead

resolveIndex :: forall a s. ConstrIndex a => IsSymbol s => Proxy a -> SProxy s -> BigInt
resolveIndex pa sps =
  let
    cn = reflectSymbol sps
    ci = constrIndex pa
  in
    case Map.lookup cn ci of
      Just i -> BigInt.fromInt i
      Nothing -> zero - BigInt.fromInt 1 -- TODO: Figure out type level

-- Instances

-- TODO: Remove Day as it's only for demo
data Day = Mon | Tue | Wed | Thurs | Fri | Sat | Sun

derive instance genericDay :: G.Generic Day _

instance ConstrIndex Day where
  constrIndex _ = Map.fromFoldable (Array.zip [ "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun" ] (0 .. 7))

instance ToData Day where
  toData = genericToData

data AnotherDay = AMon | ATue | AWed | AThurs | AFri | ASat | ASun

derive instance genericAnotherDay :: G.Generic AnotherDay _

instance ConstrIndex AnotherDay where
  constrIndex = defaultConstrIndex

instance ToData AnotherDay where
  toData = genericToData

data Tree a = Node a (Tuple (Tree a) (Tree a)) | Leaf a

derive instance genericTree :: G.Generic (Tree a) _

instance ConstrIndex (Tree a) where
  constrIndex = defaultConstrIndex

instance (ToData a) => ToData (Tree a) where
  toData x = genericToData x -- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#avoiding-stack-overflow-errors-with-recursive-types

instance ToData Void where
  toData = absurd

instance ToData Unit where
  toData _ = Constr zero []

-- NOTE: For the sake of compatibility the following toDatas have to match
-- https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-tx/src/PlutusTx/IsData/Instances.hs
instance ToData Boolean where
  toData false = Constr zero []
  toData true = Constr one []

instance ToData a => ToData (Maybe a) where
  toData (Just x) = Constr zero [ toData x ] -- Just is zero-indexed by Plutus
  toData Nothing = Constr one []

instance (ToData a, ToData b) => ToData (Either a b) where
  toData (Left e) = Constr zero [ toData e ]
  toData (Right x) = Constr one [ toData x ]

instance Fail (Text "Int is not supported, use BigInt instead") => ToData Int where
  toData = toData <<< BigInt.fromInt

instance ToData BigInt where
  toData = Integer

instance ToData UInt where
  toData = toData <<< uIntToBigInt

instance ToData a => ToData (Array a) where
  toData = List <<< map toData

instance ToData a => ToData (List a) where
  toData = foldableToPlutusData

instance (ToData a, ToData b) => ToData (a /\ b) where
  toData (a /\ b) = Constr zero [ toData a, toData b ]

instance (ToData k, ToData v) => ToData (Map k v) where
  toData mp = Map $ entries # map (toData *** toData) # Map.fromFoldable
    where
    entries = Map.toUnfoldable mp :: Array (k /\ v)

-- Note that nothing prevents the denominator from being zero, we could provide
-- safety here:
instance ToData a => ToData (Ratio a) where
  toData ratio = List [ toData (numerator ratio), toData (denominator ratio) ]

instance ToData ByteArray where
  toData = Bytes

instance ToData PlutusData where
  toData = identity

foldableToPlutusData :: forall (a :: Type) (t :: Type -> Type). Foldable t => ToData a => t a -> PlutusData
foldableToPlutusData = Array.fromFoldable >>> map toData >>> List

