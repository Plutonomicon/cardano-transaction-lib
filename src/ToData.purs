module ToData
  ( class ToDataWithIndex
  , class ToData
  , class ToDataArgs
  , genericToData
  , toData
  , toDataArgs
  , toDataWithIndex
  ) where

import Prelude

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
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Helpers (uIntToBigInt)
import Prim.TypeError (class Fail, Text)
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Constr, Integer, List, Map, Bytes))

class ToData (a :: Type) where
  toData :: a -> PlutusData

-- Generic
class ToDataWithIndex t where
  toDataWithIndex :: BigInt -> t -> PlutusData

instance toDataWithIndexSumA :: (ToDataArgs la, ToDataWithIndex (G.Sum a b))
                                => ToDataWithIndex (G.Sum (G.Constructor ln la) (G.Sum a b))  where
  toDataWithIndex n (G.Inl (G.Constructor args)) = Constr n (toDataArgs args)
  toDataWithIndex n (G.Inr x) = toDataWithIndex (n + fromInt 1) x

instance toDataWithIndexSumB :: (ToDataArgs la, ToDataArgs ra)
                                => ToDataWithIndex (G.Sum (G.Constructor ln la) (G.Constructor rn ra)) where
  toDataWithIndex n (G.Inl (G.Constructor args)) = Constr n (toDataArgs args)
  toDataWithIndex n (G.Inr (G.Constructor args)) = Constr (n + fromInt 1) (toDataArgs args)

instance (ToDataWithIndex (G.Sum l r)) => ToData (G.Sum l r) where
  toData x = toDataWithIndex zero x

-- As explained in https://harry.garrood.me/blog/write-your-own-generics/ this
-- is just a neat pattern that flattens a skewed Product of Products

class ToDataArgs a where
  toDataArgs :: a -> Array (PlutusData)

instance toDataArgsNoArguments :: ToDataArgs G.NoArguments where
  toDataArgs _ = []

instance toDataArgsArgument :: ToData a => ToDataArgs (G.Argument a) where
  toDataArgs (G.Argument x) = [ toData x ]

instance toDataArgsProduct :: (ToDataArgs a, ToDataArgs b) => ToDataArgs (G.Product a b) where
  toDataArgs (G.Product x y) = toDataArgs x <> toDataArgs y

genericToData
  :: forall a rep. G.Generic a rep => ToData rep => a -> PlutusData
genericToData = toData <<< G.from

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

