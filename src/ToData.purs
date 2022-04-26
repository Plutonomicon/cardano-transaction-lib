module ToData
  ( class ToData
  , class ToDataArgs
  , class ToDataWithSchema
  , class ToDataArgsRL
  , class ToDataArgsRL'
  , genericToData
  , toDataArgsRec
  , toDataArgsRec'
  , toData
  , toDataArgs
  , toDataWithSchema
  ) where

import Prelude

import Data.Array (cons, sortWith, reverse, snoc)
import Data.Array as Array
import Data.NonEmpty (NonEmpty)
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
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.TextEncoder (encodeUtf8)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Helpers (uIntToBigInt)
import Prim.Row as Row
import Type.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import TypeLevel.IndexedRecField
import TypeLevel.Nat (Z, S, class KnownNat, natVal)
import TypeLevel.RList
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray(ByteArray))
import Types.PlutusData (PlutusData(Constr, Integer, List, Map, Bytes))

import TypeLevel.DataSchema
-- | Classes

class ToData :: Type -> Constraint
class ToData a where
  toData :: a -> PlutusData


class ToDataWithSchema :: Type -> Type -> Constraint
class  ToDataWithSchema t a where
  toDataWithSchema :: Proxy t -> a -> PlutusData

-- As explained in https://harry.garrood.me/blog/write-your-own-generics/ this
-- is just a neat pattern that flattens a skewed Product of Products
class ToDataArgs :: Type -> Symbol -> Type -> Constraint
class IsSymbol constr <= ToDataArgs t constr a where
  toDataArgs :: Proxy t -> Proxy constr -> a -> Array (PlutusData)

-- | A helper typeclass to implement `ToDataArgs` for records.
-- Stolen from https://github.com/purescript/purescript-quickcheck/blob/v7.1.0/src/Test/QuickCheck/Arbitrary.purs#L247
class ToDataArgsRL :: forall (k :: Type). Type -> Symbol -> RL.RowList k -> Row Type -> Constraint
class ToDataArgsRL t constr list row  | t constr list -> row where
  toDataArgsRec
    :: Proxy t
    -> Proxy constr
    -> Proxy list
    -> Record row
    -> Array PlutusData

instance ToDataArgsRL' t constr list row => ToDataArgsRL t constr list row where
  toDataArgsRec proxy constr list rec = map snd <<< sortWith fst $ toDataArgsRec' proxy constr list  rec

class ToDataArgsRL' :: forall (k :: Type). Type -> Symbol -> RL.RowList k -> Row Type -> Constraint
class ToDataArgsRL' t constr list row | t constr list -> row  where
  toDataArgsRec'
    :: Proxy t
    -> Proxy constr
    -> Proxy list
    -> Record row
    -> Array (Tuple Int PlutusData)

-- | ToDataWithIndex instances for Data.Generic.Rep

instance
  ( ToDataWithSchema t l
  , ToDataWithSchema t r
  ) =>
  ToDataWithSchema t (G.Sum l r)  where
    toDataWithSchema p (G.Inl x) = toDataWithSchema p x
    toDataWithSchema p (G.Inr x) = toDataWithSchema p x

instance
  ( IsSymbol constr
  , ToDataArgs t constr arg
  , HasPlutusSchema t schema
  , ValidPlutusSchema schema list
  , GetIndexWithLabel constr list index
  , KnownNat index
  ) =>
  ToDataWithSchema t (G.Constructor constr arg)  where
  toDataWithSchema p (G.Constructor args) = Constr
    (fromInt <<< natVal $ (Proxy :: Proxy index))
    (toDataArgs p (Proxy :: Proxy constr) args)

-- | ToDataArgs instances for Data.Generic.Rep

instance IsSymbol constr => ToDataArgs a constr G.NoArguments where
  toDataArgs _ _ _  = []

instance (ToDataArgs t constr (Record row)) => ToDataArgs t constr (G.Argument (Record row)) where
  toDataArgs proxy constr (G.Argument r) = toDataArgs proxy constr r
else instance (ToData a, IsSymbol constr) => ToDataArgs x constr (G.Argument a) where
  toDataArgs _ _ (G.Argument x) = [ toData x ]

instance
  ( IsSymbol constr,
    ToDataArgsRL t constr list row,
    RL.RowToList row list
  ) =>
  ToDataArgs t constr (Record row) where
  toDataArgs proxy constr  rec =  toDataArgsRec proxy constr (Proxy :: Proxy list) rec

instance (ToDataArgs x constr a, ToDataArgs x constr b) => ToDataArgs x constr (G.Product a b) where
  toDataArgs proxy constr (G.Product x y) = toDataArgs proxy constr x <> toDataArgs proxy constr y

-- | ToDataArgsRL instances

instance ToDataArgsRL' t symbol RL.Nil ()  where
  toDataArgsRec' _ _ _ _ = []
else instance
  ( ToData a
  , ToDataArgsRL' t constr listRest rowRest
  , Row.Lacks label rowRest
  , Row.Cons label a rowRest rowFull
  , RL.RowToList rowFull (RL.Cons label a listRest)
  , IsSymbol label
  , IsSymbol constr
  , HasPlutusSchema t schema
  , ValidPlutusSchema schema rList
  , GetWithLabel constr rList rec
  , GetIndexWithLabel label rec n
  , KnownNat n
  ) =>
  ToDataArgsRL' t constr (RL.Cons label a listRest) rowFull where
  toDataArgsRec' _ constr  _ x =
    let
      keyProxy = (Proxy :: Proxy label)

      ix = natVal (Proxy :: Proxy n)

      field :: a
      field = Record.get keyProxy x
    in
      Tuple ix (toData field) `cons` toDataArgsRec' (Proxy :: Proxy t) constr (Proxy :: Proxy listRest)
        (Record.delete keyProxy x)

genericToData
  :: forall (t :: Type) (rep :: Type)
   . G.Generic t rep
  => ToDataWithSchema t rep
  => t
  -> PlutusData
genericToData = toDataWithSchema (Proxy :: Proxy t) <<< G.from

{-
resolveIndex
  :: forall (a :: Type) (s :: Symbol)
   . HasConstrIndices a
  => IsSymbol s
  => Proxy a
  -> SProxy s
  -> BigInt
resolveIndex pa sps =
  let
    cn = reflectSymbol sps
    Tuple c2i _ = constrIndices pa
  in
    case Map.lookup cn c2i of
      Just i -> BigInt.fromInt i
      Nothing -> negate one -- TODO: We should report here
-}
-- | Base ToData instances

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

instance (Foldable f, ToData a) => ToData (NonEmpty f a) where
  toData = foldableToPlutusData

instance ToData a => ToData (List a) where
  toData = foldableToPlutusData

instance (ToData a, ToData b) => ToData (Tuple a b) where
  toData (Tuple a b) = Constr zero [ toData a, toData b ]

instance (ToData k, ToData v) => ToData (Map k v) where
  toData mp = Map $ entries # map (toData *** toData)
    where
    entries = Map.toUnfoldable mp :: Array (k /\ v)

-- Note that nothing prevents the denominator from being zero, we could provide
-- safety here:
instance ToData a => ToData (Ratio a) where
  toData ratio = List [ toData (numerator ratio), toData (denominator ratio) ]

instance ToData ByteArray where
  toData = Bytes

instance ToData String where
  toData = toData <<< ByteArray <<< encodeUtf8

instance ToData PlutusData where
  toData = identity

foldableToPlutusData
  :: forall (a :: Type) (t :: Type -> Type)
   . Foldable t
  => ToData a
  => t a
  -> PlutusData
foldableToPlutusData = Array.fromFoldable >>> map toData >>> List
