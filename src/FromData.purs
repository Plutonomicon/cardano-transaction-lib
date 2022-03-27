module FromData
  ( class FromData
  , fromData
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Alternative (guard)
import Data.Array (cons, head, uncons, (:))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (repOf)
import Data.Generic.Rep as G
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Ratio (Ratio, reduce)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
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

-- Classes

class FromData (a :: Type) where
  fromData :: PlutusData -> Maybe a

class ConstrIndex (a :: Type) where
  constrIndex :: Proxy a -> Map Int String

class FromDataWithIndex a ci where
  fromDataWithIndex :: ConstrIndex ci => Proxy a -> Proxy ci -> PlutusData -> Maybe a

class FromDataArgs a where
  fromDataArgs :: Array PlutusData -> Maybe a

-- Default constructor indices
class CountedConstrIndex (a :: Type) where
  countedConstrIndex :: Proxy a -> Int -> Map Int String -> Map Int String

resolveConstr :: forall a. ConstrIndex a => Proxy a -> Int -> Maybe String
resolveConstr pa i = Map.lookup i (constrIndex pa)

-- instance FromDataArgs G.NoArguments where
--   fromDataArgs pdArgs = case uncons pdArgs of
--     Just {head: (Constr i []), tail: pds} -> Just G.NoArguments
--     Nothing -> Nothing

-- > data TestType = C0 | C1 Int | C2 Int String | C3 Int String Boolean
-- > derive instance getTestType :: Generic TestType _
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

instance (ConstrIndex a, FromDataWithIndex l a, FromDataWithIndex r a) => FromDataWithIndex (G.Sum l r) a where
  fromDataWithIndex _ pci pd = G.Inl <$> fromDataWithIndex (Proxy :: Proxy l) pci pd
    <|> G.Inr <$> fromDataWithIndex (Proxy :: Proxy r) pci pd

-- https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html
instance (IsSymbol n, ConstrIndex a, FromDataArgs arg) => FromDataWithIndex (G.Constructor n arg) a where
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

instance (FromData a) => FromData (G.Argument a) where
  fromData pd = G.Argument <$> fromData pd

instance (FromData a, FromDataArgs b) => FromDataArgs (G.Product a b) where
  fromDataArgs pdArgs = do
    { head: pd, tail: pds } <- uncons pdArgs
    repFst <- fromData pd
    repSnd <- fromDataArgs pds
    pure $ G.Product repFst repSnd

-- genericFromData :: forall a rep. G.Generic a rep => ConstrIndex a => PlutusData -> Maybe a
-- genericFromData pd = G.to <$> fromDataWithIndex (Proxy :: Proxy rep) (Proxy :: Proxy a) pd

-- Constr BigInt (Array PlutusData)

instance FromData Void where
  fromData _ = Nothing

instance FromData Unit where
  fromData (Constr n [])
    | n == zero = Just unit
  fromData _ = Nothing

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
