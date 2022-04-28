module FromData
  ( FromDataError(..)
  , class FromData
  , class FromDataArgs

  , class FromDataArgsRL
  -- , class FromDataArgsRL'
  , class FromDataWithSchema
  , fromData
  , fromDataArgs
  , fromDataArgsRec
  -- , fromDataArgsRec'
  , fromDataWithSchema
  , genericFromData
  ) where

import Prelude

import Data.Show.Generic (genericShow)
import Control.Alternative ((<|>), guard)
import Data.Array (uncons, sortWith)
import Data.Array as Array
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), hush, note)
import Data.Generic.Rep as G
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap)
import Data.Ratio (Ratio, reduce)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TextDecoder (decodeUtf8)
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

import TypeLevel.Nat
import TypeLevel.RowList.Unordered
import TypeLevel.RowList.Unordered.Indexed
import TypeLevel.DataSchema

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

{- | Replacement for 'FromDataWithIndex'. This converts a type into its Plutus Data representation with the help of a Plutus Data Schema (see TypeLevel.DataSchema)
   We cannot require that the first type argument 't' is an instance of 'HasPlutusSchema' but in practice instances of this class must have a 't' with a
   'HasPlutusSchema' instance as well.
-}
class FromDataWithSchema :: Type -> Type -> Constraint
class FromDataWithSchema t a where
  fromDataWithSchema
    :: Proxy t -> Proxy a -> PlutusData -> Maybe a

-- NOTE: Using the 'parser' approach as in https://github.com/purescript-contrib/purescript-argonaut-generic/blob/3ae9622814fd3f3f06fa8e5e58fd58d2ef256b91/src/Data/Argonaut/Decode/Generic.purs
class FromDataArgs :: Type -> Symbol -> Type -> Constraint
class FromDataArgs t c a where
  fromDataArgs
    :: Proxy t
    -> Proxy c
    -> Array PlutusData
    -> Either FromDataError { head :: a, tail :: Array PlutusData }

{- | A helper typeclass to implement `ToDataArgs` for records.
   Adapted from https://github.com/purescript/purescript-quickcheck/blob/v7.1.0/src/Test/QuickCheck/Arbitrary.purs#L247

   The second argument is a symbol which represents the name of a record constructor.

   The third argument to the class is an @RList@ - an *unordered* version of RowList. See TypeLevel.RList for details
-}
class FromDataArgsRL :: Type -> Symbol -> RowList Type -> Row Type -> Constraint
class FromDataArgsRL t constr list row | t constr list -> row where
  fromDataArgsRec
    :: forall (rlproxy :: RowList Type -> Type)
     . Proxy t
    -> Proxy constr
    -> rlproxy list
    -> Array PlutusData
    -> Either FromDataError { head :: Record row, tail :: Array PlutusData }

-- | FromDataWithIndex instances for Data.Generic.Rep
-- See https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html

instance
  ( FromDataWithSchema t l
  , FromDataWithSchema t r
  ) =>
  FromDataWithSchema t (G.Sum l r) where
  fromDataWithSchema _ pci pd =
    G.Inl <$> fromDataWithSchema (Proxy :: Proxy t) (Proxy :: Proxy l) pd
      <|> G.Inr <$> fromDataWithSchema (Proxy :: Proxy t) (Proxy :: Proxy r) pd

else instance
  ( IsSymbol constr
  , HasPlutusSchema t schema
  , ValidPlutusSchema schema rList
  , GetIndexWithLabel constr rList ix
  , FromDataArgs t constr args
  , KnownNat ix
  ) =>
  FromDataWithSchema t (G.Constructor constr args) where
  fromDataWithSchema _ _ (Constr i pdArgs) = do
    ix <- BigInt.toInt i
    -- TODO: Add err reporting to FromDataWithIndex
    guard $ natVal (Proxy :: Proxy ix) == ix
    { head: repArgs, tail: pdArgs' } <- hush $ fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy constr)
      pdArgs
    guard $ pdArgs' == []
    pure $ G.Constructor repArgs
  fromDataWithSchema _ _ _ = Nothing

else instance
  ( FromDataWithSchema t a
  ) =>
  FromDataWithSchema t (G.Argument a) where
  fromDataWithSchema _ _ pd = G.Argument <$> fromDataWithSchema
    (Proxy :: Proxy t)
    (Proxy :: Proxy a)
    pd

-- | FromDataArgs instance for Data.Generic.Rep

instance FromDataArgs t c (G.NoArguments) where
  fromDataArgs _ _ [] = Right { head: G.NoArguments, tail: [] }
  fromDataArgs _ _ pdArgs = Left $ ArgsWantedButGot 0 pdArgs

instance
  ( FromDataArgsRL t constr rList row
  , HasPlutusSchema t schema
  , ValidPlutusSchema schema rrList
  , GetWithLabel constr rrList rList
  ) =>
  FromDataArgs t constr (G.Argument (Record row)) where
  fromDataArgs _ constr pdArgs = do
    { head, tail } <- fromDataArgsRec (Proxy :: Proxy t) (Proxy :: Proxy constr)
      (Proxy :: Proxy rList)
      pdArgs
    pure { head: G.Argument head, tail }
else instance (FromData a) => FromDataArgs t constr (G.Argument a) where
  fromDataArgs _ _ pdArgs = do
    { head: pd, tail: pds } <- note (ArgsWantedButGot 1 pdArgs) $ uncons pdArgs
    repArg <- note (FromDataFailed pd) $ fromData pd
    pure $ { head: G.Argument repArg, tail: pds }

instance
  ( FromDataArgs t c a
  , FromDataArgs t c b
  ) =>
  FromDataArgs t c (G.Product a b) where
  fromDataArgs _ constr pdArgs = do
    { head: repFst, tail: pdArgs' } <- fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy c)
      pdArgs
    { head: repSnd, tail: pdArgs'' } <- fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy c)
      pdArgs'
    pure $ { head: G.Product repFst repSnd, tail: pdArgs'' }

-- | FromDataArgsRL instances

instance FromDataArgsRL t c Nil () where
  fromDataArgsRec _ _ _ [] = Right { head: {}, tail: [] }
  fromDataArgsRec _ _ _ pdArgs = Left $ ArgsWantedButGot 0 pdArgs

instance
  ( FromData a
  , FromDataArgsRL t constr rListRest rowRest
  , Row.Lacks key rowRest
  , Row.Cons key a rowRest rowFull
  , IsSymbol key
  ) =>
  FromDataArgsRL t constr (Cons key a rListRest) rowFull where
  fromDataArgsRec _ _ _ pdArgs = do
    let keyProxy = Proxy :: Proxy key
    { head: pdArg, tail: pdArgs' } <- note (ArgsWantedButGot 1 pdArgs) $ uncons
      pdArgs
    field <- note (FromDataFailed pdArg) $ fromData pdArg
    { head: rec, tail: pdArgs'' } <- fromDataArgsRec (Proxy :: Proxy t)
      (Proxy :: Proxy constr)
      (Proxy :: Proxy rListRest)
      pdArgs'
    pure $
      { head: (Record.insert keyProxy field rec)
      , tail: pdArgs''
      }

genericFromData
  :: forall (t :: Type) (rep :: Type)
   . G.Generic t rep
  => FromDataWithSchema t rep
  => PlutusData
  -> Maybe t
genericFromData pd = G.to <$> fromDataWithSchema (Proxy :: Proxy t)
  (Proxy :: Proxy rep)
  pd

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

instance FromData a => FromData (NonEmpty Array a) where
  fromData d = do
    { head, tail } <- Array.uncons =<< fromData d
    pure $ NonEmpty head tail

instance FromData a => FromData (List a) where
  fromData = fromDataUnfoldable

instance (FromData a, FromData b) => FromData (Tuple a b) where
  fromData (Constr n [ a, b ])
    | n == zero = Tuple <$> fromData a <*> fromData b
  fromData _ = Nothing

instance (FromData k, Ord k, FromData v) => FromData (Map k v) where
  fromData (Map mp) = do
    Map.fromFoldable <$> for mp \(k /\ v) ->
      Tuple <$> fromData k <*> fromData v
  fromData _ = Nothing

instance FromData ByteArray where
  fromData (Bytes res) = Just res
  fromData _ = Nothing

instance FromData String where
  fromData (Bytes bytes) = hush $ decodeUtf8 $ unwrap bytes
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
