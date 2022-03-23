module ToData
  ( AnotherDay(..)
  , Day(..)
  , Tree(..)
  , class ConstrIndex
  , class ToData
  , class ToDataArgs
  , class ToDataWithIndex
  , class CountedConstrIndex
  , defaultMakeConstrIndex
  , defaultResolveIndex
  , genericToData
  , genericToDataWithIndex
  , makeConstrIndex
  , resolveIndex
  , toData
  , toDataArgs
  , toDataWithIndex
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Foldable (class Foldable)
import Data.Generic.Rep as G
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Profunctor.Strong ((***))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Helpers (uIntToBigInt)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Constr, Integer, List, Map, Bytes))
import Data.Tuple (Tuple)

-- Classes

class ToData (a :: Type) where
  toData :: a -> PlutusData

class ConstrIndex (a :: Type) where
  resolveIndex :: forall s. IsSymbol s => Proxy a -> SProxy s -> BigInt

class ToDataWithIndex a ci where
  toDataWithIndex :: ConstrIndex ci => Proxy ci -> a -> PlutusData

-- As explained in https://harry.garrood.me/blog/write-your-own-generics/ this
-- is just a neat pattern that flattens a skewed Product of Products
class ToDataArgs a where
  toDataArgs :: a -> Array (PlutusData)

-- Default constructor indices
class CountedConstrIndex (a :: Type) where
  makeConstrIndex :: Proxy a -> Int -> Map String Int -> Map String Int

-- Data.Generic.Rep instances

instance toDataWithIndexSum ::
  ( ConstrIndex ci
  , ToDataWithIndex l ci
  , ToDataWithIndex r ci
  ) =>
  ToDataWithIndex (G.Sum l r) ci where
  toDataWithIndex p (G.Inl x) = toDataWithIndex (p :: Proxy ci) x
  toDataWithIndex p (G.Inr x) = toDataWithIndex (p :: Proxy ci) x

instance toDataWithIndexConstr ::
  ( IsSymbol ln
  , ConstrIndex ci
  , ToDataArgs la
  ) =>
  ToDataWithIndex (G.Constructor ln la) ci where
  toDataWithIndex p (G.Constructor args) = Constr (resolveIndex p (SProxy :: SProxy ln)) (toDataArgs args)

instance toDataArgsNoArguments :: ToDataArgs G.NoArguments where
  toDataArgs _ = []

instance toDataArgsArgument :: ToData a => ToDataArgs (G.Argument a) where
  toDataArgs (G.Argument x) = [ toData x ]

instance toDataArgsProduct :: (ToDataArgs a, ToDataArgs b) => ToDataArgs (G.Product a b) where
  toDataArgs (G.Product x y) = toDataArgs x <> toDataArgs y

instance countedConstrIndexSum ::
  ( CountedConstrIndex a
  , CountedConstrIndex b
  ) =>
  CountedConstrIndex (G.Sum a b) where
  makeConstrIndex _ i sym2ix = makeConstrIndex (Proxy :: Proxy b) (i + 1) (makeConstrIndex (Proxy :: Proxy a) i sym2ix)

instance countedConstrIndexConstr :: (IsSymbol ln) => CountedConstrIndex (G.Constructor ln la) where
  makeConstrIndex _ i sym2ix = (Map.insert (reflectSymbol (SProxy :: SProxy ln)) i sym2ix)

genericToData
  :: forall a rep. G.Generic a rep => ToData rep => a -> PlutusData
genericToData = toData <<< G.from

genericToDataWithIndex
  :: forall a rep. G.Generic a rep => ConstrIndex a => ToDataWithIndex rep a => a -> PlutusData
genericToDataWithIndex = toDataWithIndex (Proxy :: Proxy a) <<< G.from

defaultMakeConstrIndex :: forall a rep. G.Generic a rep => CountedConstrIndex rep => Proxy a -> Map String Int
defaultMakeConstrIndex _ = makeConstrIndex (Proxy :: Proxy rep) 0 Map.empty

defaultResolveIndex :: forall a rep s. G.Generic a rep => CountedConstrIndex rep => IsSymbol s => Proxy a -> SProxy s -> BigInt
defaultResolveIndex _ constrSym =
  let
    constrName = reflectSymbol constrSym
    constrIndex = defaultMakeConstrIndex (Proxy :: Proxy a)
  in
    case Map.lookup constrName constrIndex of
      Just i -> BigInt.fromInt i
      Nothing -> zero - BigInt.fromInt 1

-- Instances

-- TODO: Remove Day as it's only for demo
data Day = Mon | Tue | Wed | Thurs | Fri | Sat | Sun

derive instance genericDay :: G.Generic Day _

instance ConstrIndex Day where
  resolveIndex _ constrSymb = case reflectSymbol constrSymb of
    "Mon" -> zero
    "Tue" -> one
    "Wed" -> BigInt.fromInt 2
    "Thurs" -> BigInt.fromInt 3
    "Fri" -> BigInt.fromInt 4
    "Sat" -> BigInt.fromInt 5
    "Sun" -> BigInt.fromInt 6
    _ -> zero - BigInt.fromInt 1

instance ToData Day where
  toData = genericToDataWithIndex

data AnotherDay = AMon | ATue | AWed | AThurs | AFri | ASat | ASun

derive instance genericAnotherDay :: G.Generic AnotherDay _

instance ConstrIndex AnotherDay where
  resolveIndex = defaultResolveIndex

instance ToData AnotherDay where
  toData = genericToDataWithIndex

data Tree a = Node a (Tuple (Tree a) (Tree a)) | Leaf a

derive instance genericTree :: G.Generic (Tree a) _

instance ConstrIndex (Tree a) where
  resolveIndex = defaultResolveIndex

instance (ToData a) => ToData (Tree a) where
  toData x = genericToDataWithIndex x -- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#avoiding-stack-overflow-errors-with-recursive-types

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

