module FromData
  ( FromDataError(..)
  , class FromData
  , class FromDataArgs
  , class FromDataArgsRL
  , class FromDataWithIndex
  , fromData
  , fromDataArgs
  , fromDataArgsRec
  , fromDataWithIndex
  , genericFromData
  ) where

import Prelude

import ConstrIndices (class HasConstrIndices, constrIndices)
import Data.Show.Generic (genericShow)
import Control.Alternative ((<|>), guard)
import Data.Array (uncons)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), hush, note)
import Data.Generic.Rep as G
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Ratio (Ratio, reduce)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.Unfoldable (class Unfoldable)
import Helpers (bigIntToUInt)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData(Bytes, Constr, List, Map, Integer))

-- | Errors
data FromDataError
  = ArgsWantedButGot Int (Array PlutusData)
  | FromDataFailed PlutusData

derive instance G.Generic FromDataError _

instance Show FromDataError where
  show = genericShow

-- | Classes

class FromData :: Type -> Constraint
class FromData a where
  fromData :: PlutusData -> Maybe a

class FromDataWithIndex :: Type -> Type -> Constraint
class HasConstrIndices ci <= FromDataWithIndex a ci where
  fromDataWithIndex
    :: Proxy a -> Proxy ci -> PlutusData -> Maybe a

-- NOTE: Using the 'parser' approach as in https://github.com/purescript-contrib/purescript-argonaut-generic/blob/3ae9622814fd3f3f06fa8e5e58fd58d2ef256b91/src/Data/Argonaut/Decode/Generic.purs
class FromDataArgs :: Type -> Constraint
class FromDataArgs a where
  fromDataArgs
    :: Array PlutusData
    -> Either FromDataError { head :: a, tail :: Array PlutusData }

-- | A helper typeclass to implement `ToDataArgs` for records.
-- Stolen from https://github.com/purescript/purescript-quickcheck/blob/v7.1.0/src/Test/QuickCheck/Arbitrary.purs#L247
class FromDataArgsRL :: RL.RowList Type -> Row Type -> Constraint
class FromDataArgsRL list row | list -> row where
  fromDataArgsRec
    :: forall (rlproxy :: RL.RowList Type -> Type)
     . rlproxy list
    -> Array PlutusData
    -> Either FromDataError { head :: Record row, tail :: Array PlutusData }

-- | FromDataWithIndex instances for Data.Generic.Rep
-- See https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html

instance
  ( FromDataWithIndex l a
  , FromDataWithIndex r a
  ) =>
  FromDataWithIndex (G.Sum l r) a where
  fromDataWithIndex _ pci pd =
    G.Inl <$> fromDataWithIndex (Proxy :: Proxy l) pci pd
      <|> G.Inr <$> fromDataWithIndex (Proxy :: Proxy r) pci pd

instance
  ( IsSymbol n
  , HasConstrIndices a
  , FromDataArgs arg
  ) =>
  FromDataWithIndex (G.Constructor n arg) a where
  fromDataWithIndex _ pci (Constr i pdArgs) = do
    ix <- BigInt.toInt i
    cn <- resolveConstr pci ix
    let rn = reflectSymbol (Proxy :: Proxy n)
    -- TODO: Add err reporting to FromDataWithIndex
    guard $ cn == rn
    { head: repArgs, tail: pdArgs' } <- hush $ fromDataArgs pdArgs
    guard $ pdArgs' == []
    pure $ G.Constructor repArgs
  fromDataWithIndex _ _ _ = Nothing

instance
  ( HasConstrIndices ci
  , FromDataWithIndex a ci
  ) =>
  FromDataWithIndex (G.Argument a) ci where
  fromDataWithIndex _ pci pd = G.Argument <$> fromDataWithIndex
    (Proxy :: Proxy a)
    pci
    pd

-- | FromDataArgs instance for Data.Generic.Rep

instance FromDataArgs (G.NoArguments) where
  fromDataArgs [] = Right { head: G.NoArguments, tail: [] }
  fromDataArgs pdArgs = Left $ ArgsWantedButGot 0 pdArgs

instance
  ( FromDataArgsRL list row
  , RL.RowToList row list
  ) =>
  FromDataArgs (G.Argument (Record row)) where
  fromDataArgs pdArgs = do
    { head, tail } <- fromDataArgsRec (Proxy :: Proxy list) pdArgs
    pure { head: G.Argument head, tail }
else instance (FromData a) => FromDataArgs (G.Argument a) where
  fromDataArgs pdArgs = do
    { head: pd, tail: pds } <- note (ArgsWantedButGot 1 pdArgs) $ uncons pdArgs
    repArg <- note (FromDataFailed pd) $ fromData pd
    pure $ { head: G.Argument repArg, tail: pds }

instance (FromDataArgs a, FromDataArgs b) => FromDataArgs (G.Product a b) where
  fromDataArgs pdArgs = do
    { head: repFst, tail: pdArgs' } <- fromDataArgs pdArgs
    { head: repSnd, tail: pdArgs'' } <- fromDataArgs pdArgs'
    pure $ { head: G.Product repFst repSnd, tail: pdArgs'' }

-- | FromDataArgsRL instances

instance FromDataArgsRL RL.Nil () where
  fromDataArgsRec _ [] = Right { head: {}, tail: [] }
  fromDataArgsRec _ pdArgs = Left $ ArgsWantedButGot 0 pdArgs

instance
  ( FromData a
  , FromDataArgsRL listRest rowRest
  , Row.Lacks key rowRest
  , Row.Cons key a rowRest rowFull
  , RL.RowToList rowFull (RL.Cons key a listRest)
  , IsSymbol key
  ) =>
  FromDataArgsRL (RL.Cons key a listRest) rowFull where
  fromDataArgsRec _ pdArgs = do
    let keyProxy = Proxy :: Proxy key
    { head: pdArg, tail: pdArgs' } <- note (ArgsWantedButGot 1 pdArgs) $ uncons
      pdArgs
    field <- note (FromDataFailed pdArg) $ fromData pdArg
    { head: rec, tail: pdArgs'' } <- fromDataArgsRec (Proxy :: Proxy listRest)
      pdArgs'
    pure $
      { head: (Record.insert keyProxy field rec)
      , tail: pdArgs''
      }

genericFromData
  :: forall (a :: Type) (rep :: Type)
   . G.Generic a rep
  => FromDataWithIndex rep a
  => PlutusData
  -> Maybe a
genericFromData pd = G.to <$> fromDataWithIndex (Proxy :: Proxy rep)
  (Proxy :: Proxy a)
  pd

resolveConstr
  :: forall (a :: Type). HasConstrIndices a => Proxy a -> Int -> Maybe String
resolveConstr pa i = let Tuple _ i2c = constrIndices pa in Map.lookup i i2c

-- | Base FromData instances

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

instance (FromData a, FromData b) => FromData (Tuple a b) where
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

fromDataUnfoldable
  :: forall (a :: Type) (t :: Type -> Type)
   . Unfoldable t
  => FromData a
  => PlutusData
  -> Maybe (t a)
fromDataUnfoldable (List entries) = Array.toUnfoldable <$> traverse fromData
  entries
fromDataUnfoldable _ = Nothing
