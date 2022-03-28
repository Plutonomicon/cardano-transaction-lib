module FromData
  ( class FromData
  , fromData
  , genericFromData
  , class FromDataWithIndex
  , fromDataWithIndex
  , class FromDataArgs
  , fromDataArgs
  ) where

import Prelude

import ConstrIndex (class HasConstrIndex, constrIndex)
import Control.Alternative ((<|>), guard)
import Data.Array (uncons)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Generic.Rep as G
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Ratio (Ratio, reduce)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.Unfoldable (class Unfoldable)
import Helpers (bigIntToUInt)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Bytes, Constr, List, Map, Integer))

-- | Classes

class FromData (a :: Type) where
  fromData :: PlutusData -> Maybe a

class FromDataWithIndex a ci where
  fromDataWithIndex :: HasConstrIndex ci => Proxy a -> Proxy ci -> PlutusData -> Maybe a

class FromDataArgs a where
  fromDataArgs :: Array PlutusData -> Maybe a

-- > data TestType = C0 | C1 Int | C2 Int String | C3 Int String Boolean | C4 Int TestType
-- > derive instance Generic TestType _
-- > :t (from C0)
-- Sum
--  (Constructor "C0" NoArguments)
--  (Sum
--     (Constructor "C1" (Argument Int))
--     (Sum
--       (Constructor "C2"
--         (Product (Argument Int) (Argument String)))
--       (Constructor "C3"
--         (Product
--           (Argument Int)
--           (Product
--             (Argument String)
--             (Argument Boolean))))))

-- | Data.Generic.Rep instances

instance (HasConstrIndex a, FromDataWithIndex l a, FromDataWithIndex r a) => FromDataWithIndex (G.Sum l r) a where
  fromDataWithIndex _ pci pd = G.Inl <$> fromDataWithIndex (Proxy :: Proxy l) pci pd
    <|> G.Inr <$> fromDataWithIndex (Proxy :: Proxy r) pci pd

-- https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html
instance (IsSymbol n, HasConstrIndex a, FromDataArgs arg) => FromDataWithIndex (G.Constructor n arg) a where
  fromDataWithIndex _ pci (Constr i pdArgs) = do
    ix <- BigInt.toInt i
    cn <- resolveConstr pci ix
    let rn = reflectSymbol (Proxy :: Proxy n)
    guard $ cn == rn
    repArgs <- fromDataArgs pdArgs
    pure $ (G.Constructor repArgs :: G.Constructor n arg)
  fromDataWithIndex _ _ _ = Nothing

instance FromDataArgs (G.NoArguments) where
  fromDataArgs [] = Just G.NoArguments
  fromDataArgs _ = Nothing

instance (HasConstrIndex ci, FromDataWithIndex a ci) => FromDataWithIndex (G.Argument a) ci where
  fromDataWithIndex _ pci pd = G.Argument <$> fromDataWithIndex (Proxy :: Proxy a) pci pd

instance (FromData a) => FromDataArgs (G.Argument a) where
  fromDataArgs pdArgs = do
    { head: pd, tail: pds } <- uncons pdArgs
    guard $ pds == []
    repArg <- fromData pd
    pure $ G.Argument repArg

instance (FromDataArgs a, FromDataArgs b) => FromDataArgs (G.Product a b) where
  fromDataArgs pdArgs = do
    { head: pd, tail: pds } <- uncons pdArgs
    repFst <- fromDataArgs [ pd ]
    repSnd <- fromDataArgs pds
    pure $ G.Product repFst repSnd

genericFromData :: forall a rep. G.Generic a rep => HasConstrIndex a => FromDataWithIndex rep a => PlutusData -> Maybe a
genericFromData pd = G.to <$> fromDataWithIndex (Proxy :: Proxy rep) (Proxy :: Proxy a) pd

resolveConstr :: forall a. HasConstrIndex a => Proxy a -> Int -> Maybe String
resolveConstr pa i = let Tuple _ i2c = constrIndex pa in Map.lookup i i2c

-- | Base instances

instance FromData Void where
  fromData _ = Nothing

instance FromData Unit where
  fromData (Constr n [])
    | n == zero = Just unit
  fromData _ = Nothing

-- NOTE: For the sake of compatibility the following fromDatas have to match
-- https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-tx/src/PlutusTx/IsData/Instances.hs
instance FromData Boolean where
  fromData (Constr n [])
    | n == zero = Just false
    | n == one = Just true
  fromData _ = Nothing

instance FromData a => FromData (Maybe a) where
  fromData (Constr n [ pd ])
    | n == zero = maybe Nothing (Just <<< Just) (fromData pd) -- Just is zero-indexed by Plutus
  fromData (Constr n [])
    | n == one = Just Nothing
  fromData _ = Nothing

instance (FromData a, FromData b) => FromData (Either a b) where
  fromData (Constr n [ pd ])
    | n == zero = maybe Nothing (Just <<< Left) (fromData pd)
    | n == one = maybe Nothing (Just <<< Right) (fromData pd)
  fromData _ = Nothing

instance Fail (Text "Int is not supported, use BigInt instead") => FromData Int where
  fromData _ = Nothing

instance FromData BigInt where
  fromData (Integer n) = Just n
  fromData _ = Nothing

instance FromData UInt where
  fromData (Integer n) = bigIntToUInt n
  fromData _ = Nothing

instance FromData a => FromData (Array a) where
  fromData = fromDataUnfoldable

instance FromData a => FromData (List a) where
  fromData = fromDataUnfoldable

instance (FromData a, FromData b) => FromData (a /\ b) where
  fromData (Constr n [ a, b ])
    | n == zero = Tuple <$> fromData a <*> fromData b
  fromData _ = Nothing

instance (FromData k, Ord k, FromData v) => FromData (Map k v) where
  fromData (Map mp) = do
    Map.fromFoldable <$> for (Map.toUnfoldable mp :: Array _) \(k /\ v) ->
      Tuple <$> fromData k <*> fromData v
  fromData _ = Nothing

instance FromData ByteArray where
  fromData (Bytes res) = Just res
  fromData _ = Nothing

-- Nothing prevents fromData b ~ Maybe BigInt from being zero here, perhaps
-- we want more safety:
instance (Ord a, EuclideanRing a, FromData a) => FromData (Ratio a) where
  fromData (List [ a, b ]) = reduce <$> fromData a <*> fromData b
  fromData _ = Nothing

instance FromData PlutusData where
  fromData = Just

fromDataUnfoldable :: forall (a :: Type) (t :: Type -> Type). Unfoldable t => FromData a => PlutusData -> Maybe (t a)
fromDataUnfoldable (List entries) = Array.toUnfoldable <$> traverse fromData entries
fromDataUnfoldable _ = Nothing
