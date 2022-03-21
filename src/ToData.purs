module ToData
  ( Day(..)
  , class ConstrIndex
  , class IsIndexed
  , class ToData
  , class ToDataArgs
  , genericIndexFromConstructor
  , genericToData
  , indexFromConstructor
  , toData
  , toDataArgs
  , constrToIndex
  )
  where

import Prelude

import Contract.Prelude (Tuple(..), genericShow)
import Data.Array as Array
import Data.BigInt (BigInt, fromInt)
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
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Helpers (uIntToBigInt)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Constr, Integer, List, Map, Bytes))

data Day = Mon | Tue | Wed | Thurs | Fri | Sat | Sun
derive instance genericDay :: G.Generic Day _

instance showDay :: Show Day where
  show = genericShow

instance dayToData :: ToData Day where
  toData = genericToData

class ToData (a :: Type) where
  toData :: a -> PlutusData

class ConstrIndex (a :: Type) where
  constrToIndex :: Proxy a -> Map String Int

instance dayConstrIndex :: ConstrIndex Day where
  constrToIndex _ = Map.fromFoldable [Tuple "Mon" 0]

-- Generic
class ToDataArgs a where
  toDataArgs :: a -> Array (PlutusData)

instance toDataArgsNoArguments :: ToDataArgs G.NoArguments where
  toDataArgs _ = []

instance toDataArgsArgument :: ToData a => ToDataArgs (G.Argument a) where
  toDataArgs (G.Argument x) = [toData x]

instance toDataArgsProduct :: (ToDataArgs a, ToDataArgs b) => ToDataArgs (G.Product a b) where
  toDataArgs (G.Product x y) = toDataArgs x <> toDataArgs y

instance toDataSum' :: (ToDataArgs largs, ToDataArgs rargs) => ToData (G.Sum (G.Constructor lname largs) (G.Constructor rname rargs)) where
  toData (G.Inl (G.Constructor args)) = Constr zero (toDataArgs args)
  toData (G.Inr (G.Constructor args)) = Constr one (toDataArgs args)

instance toDataSum'' :: (ToDataArgs args, ToData (G.Sum l r)) => ToData (G.Sum (G.Constructor name args) (G.Sum l r)) where
  toData (G.Inl (G.Constructor args)) = Constr zero (toDataArgs args)
  toData (G.Inr x) = toData x

genericToData
  :: forall a rep. G.Generic a rep => ToData rep => a -> PlutusData
genericToData = toData <<< G.from

class IsIndexed (a :: Type) where
  indexFromConstructor :: forall name. IsSymbol name => IsIndexed a => Int -> SProxy name -> Proxy a -> BigInt

instance sumIsIndexed :: (IsSymbol ln, IsSymbol rn) => IsIndexed (G.Sum (G.Constructor ln la) (G.Constructor rn ra)) where
  indexFromConstructor i sname _ = if reflectSymbol sname == reflectSymbol (Proxy :: Proxy ln)
                                   then fromInt i
                                   else if reflectSymbol sname == reflectSymbol (Proxy :: Proxy rn)
                                        then (fromInt i + fromInt 1)
                                        else BigInt.fromInt (-1)

instance sumIsIndexed' :: (IsSymbol ln, IsIndexed (G.Sum l r)) => IsIndexed (G.Sum (G.Constructor ln la) (G.Sum l r)) where
  indexFromConstructor i constrName _ = if reflectSymbol constrName == reflectSymbol (Proxy :: Proxy ln)
                                        then fromInt i
                                        else indexFromConstructor (i + 1) constrName (Proxy :: Proxy (G.Sum l r))

genericIndexFromConstructor
  :: forall a rep name. G.Generic a rep => IsIndexed rep => IsSymbol name => SProxy name -> Proxy a -> BigInt
genericIndexFromConstructor sname x = indexFromConstructor 0 sname (G.from <$> x)

-- Instances
instance ToData Void where
  toData = absurd

instance ToData Unit where
  toData _ = Constr zero []

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

