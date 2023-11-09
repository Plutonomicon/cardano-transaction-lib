module Ctl.Internal.FromData
  ( FromDataError
      ( ArgsWantedButGot
      , FromDataFailed
      , BigNumToIntFailed
      , IndexWantedButGot
      , WantedConstrGot
      )
  , class FromData
  , class FromDataArgs
  , class FromDataArgsRL
  , class FromDataWithSchema
  , fromData
  , fromDataArgs
  , fromDataArgsRec
  , fromDataWithSchema
  , genericFromData
  ) where

import Prelude

import Contract.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Contract.Crypto.Secp256k1.ECDSA (ECDSAPublicKey)
import Control.Alternative ((<|>))
import Ctl.Internal.Helpers (bigIntToUInt)
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , class ValidPlutusSchema
  )
import Ctl.Internal.TypeLevel.Nat (class KnownNat, natVal)
import Ctl.Internal.TypeLevel.RowList.Unordered.Indexed
  ( class GetIndexWithLabel
  , class GetWithLabel
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.PlutusData (PlutusData(Bytes, Constr, List, Integer))
import Ctl.Internal.Types.RawBytes (RawBytes)
import Data.Array (uncons)
import Data.Array as Array
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right), hush, note)
import Data.Generic.Rep as G
import Data.List (List)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.Ratio (Ratio, reduce)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TextDecoder (decodeUtf8)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.UInt (UInt)
import Data.Unfoldable (class Unfoldable)
import JS.BigInt (BigInt)
import Noble.Secp256k1.ECDSA
  ( ECDSASignature
  , MessageHash
  , mkECDSAPublicKey
  , mkMessageHash
  , mkPrivateKey
  )
import Noble.Secp256k1.Schnorr
  ( SchnorrPublicKey
  , SchnorrSignature
  , mkSchnorrPublicKey
  )
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(Proxy))

-- | Errors
data FromDataError
  = ArgsWantedButGot String Int (Array PlutusData)
  | FromDataFailed String PlutusData
  | BigNumToIntFailed String BigNum
  | IndexWantedButGot String Int Int
  | WantedConstrGot String PlutusData

derive instance G.Generic FromDataError _

instance Show FromDataError where
  show = genericShow

noteB :: forall (a :: Type). a -> Boolean -> Either a Unit
noteB _ true = pure unit
noteB n false = Left n

-- | Classes

class FromData :: Type -> Constraint
class FromData a where
  fromData :: PlutusData -> Maybe a

-- | Replacement for 'FromDataWithIndex'. This converts a type into its Plutus Data representation with the help of a Plutus Data Schema (see TypeLevel.DataSchema)
-- | We cannot require that the first type argument 't' is an instance of 'HasPlutusSchema' but in practice instances of this class must have a 't' with a
-- | 'HasPlutusSchema' instance as well.
class FromDataWithSchema :: Type -> Type -> Constraint
class FromDataWithSchema t a where
  fromDataWithSchema
    :: Proxy t -> Proxy a -> PlutusData -> Either FromDataError a

-- NOTE: Using the 'parser' approach as in https://github.com/purescript-contrib/purescript-argonaut-generic/blob/3ae9622814fd3f3f06fa8e5e58fd58d2ef256b91/src/Data/Argonaut/Decode/Generic.purs
class FromDataArgs :: Type -> Symbol -> Type -> Constraint
class FromDataArgs t c a where
  fromDataArgs
    :: IsSymbol c
    => Proxy t
    -> Proxy c
    -> Array PlutusData
    -> Either FromDataError { head :: a, tail :: Array PlutusData }

-- | A helper typeclass to implement `FromDataArgs` for records.
-- | Adapted from https://github.com/purescript/purescript-quickcheck/blob/v7.1.0/src/Test/QuickCheck/Arbitrary.purs#L247
-- |
-- | The second argument is a symbol which represents the name of a record constructor.
-- |
-- | The third argument to the class is a RowList.

class FromDataArgsRL
  :: Type -> Symbol -> RL.RowList Type -> Row Type -> Constraint
class FromDataArgsRL t constr list row | t constr list -> row where
  fromDataArgsRec
    :: forall (rlproxy :: RL.RowList Type -> Type)
     . IsSymbol constr
    => Proxy t
    -> Proxy constr
    -> rlproxy list
    -> Array PlutusData
    -> Either FromDataError { head :: Record row, tail :: Array PlutusData }

-- | FromDataWithSchema instances for Data.Generic.Rep
-- | See https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html

instance
  ( FromDataWithSchema t l
  , FromDataWithSchema t r
  ) =>
  FromDataWithSchema t (G.Sum l r) where
  fromDataWithSchema _ _ pd =
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
    let constrName = reflectSymbol (Proxy :: Proxy constr)
    gotIx <- note (BigNumToIntFailed constrName i) (BigNum.toInt i)
    wantedIx <- pure $ natVal (Proxy :: Proxy ix)
    noteB (IndexWantedButGot constrName wantedIx gotIx) (wantedIx == gotIx)
    { head: repArgs, tail: pdArgs' } <- fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy constr)
      pdArgs
    noteB (ArgsWantedButGot constrName 0 pdArgs') (pdArgs' == [])
    pure $ G.Constructor repArgs
  fromDataWithSchema _ _ pd = Left $ WantedConstrGot
    (reflectSymbol (Proxy :: Proxy constr))
    pd
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
  fromDataArgs _ c pdArgs = Left $ ArgsWantedButGot (reflectSymbol c) 0 pdArgs

instance
  ( FromDataArgsRL t constr rList row
  , HasPlutusSchema t schema
  , ValidPlutusSchema schema rrList
  , GetWithLabel constr rrList rList
  ) =>
  FromDataArgs t constr (G.Argument (Record row)) where
  fromDataArgs _ _ pdArgs = do
    { head, tail } <- fromDataArgsRec (Proxy :: Proxy t) (Proxy :: Proxy constr)
      (Proxy :: Proxy rList)
      pdArgs
    pure { head: G.Argument head, tail }
else instance (FromData a) => FromDataArgs t constr (G.Argument a) where
  fromDataArgs _ _ pdArgs = do
    let constrName = reflectSymbol (Proxy :: Proxy constr)
    { head: pd, tail: pds } <- note (ArgsWantedButGot constrName 1 pdArgs) $
      uncons pdArgs
    repArg <- note (FromDataFailed constrName pd) $ fromData pd
    pure { head: G.Argument repArg, tail: pds }

instance
  ( FromDataArgs t c a
  , FromDataArgs t c b
  ) =>
  FromDataArgs t c (G.Product a b) where
  fromDataArgs _ _ pdArgs = do
    { head: repFst, tail: pdArgs' } <- fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy c)
      pdArgs
    { head: repSnd, tail: pdArgs'' } <- fromDataArgs (Proxy :: Proxy t)
      (Proxy :: Proxy c)
      pdArgs'
    pure { head: G.Product repFst repSnd, tail: pdArgs'' }

-- | FromDataArgsRL instances

instance FromDataArgsRL t constr RL.Nil () where
  fromDataArgsRec _ _ _ [] = Right { head: {}, tail: [] }
  fromDataArgsRec _ c _ pdArgs = Left $ ArgsWantedButGot (reflectSymbol c) 0
    pdArgs

instance
  ( FromData a
  , FromDataArgsRL t constr rListRest rowRest
  , Row.Lacks key rowRest
  , Row.Cons key a rowRest rowFull
  , IsSymbol key
  ) =>
  FromDataArgsRL t constr (RL.Cons key a rListRest) rowFull where
  fromDataArgsRec _ c _ pdArgs = do
    let keyProxy = Proxy :: Proxy key
    { head: pdArg, tail: pdArgs' } <-
      note (ArgsWantedButGot (reflectSymbol c) 1 pdArgs) $ uncons
        pdArgs
    field <- note (FromDataFailed (reflectSymbol c) pdArg) $ fromData pdArg
    { head: rec, tail: pdArgs'' } <- fromDataArgsRec (Proxy :: Proxy t)
      (Proxy :: Proxy constr)
      (Proxy :: Proxy rListRest)
      pdArgs'
    pure $
      { head: (Record.insert keyProxy field rec)
      , tail: pdArgs''
      }

-- TODO: Unhush the errors in FromData
genericFromData
  :: forall (t :: Type) (rep :: Type)
   . G.Generic t rep
  => FromDataWithSchema t rep
  => PlutusData
  -> Maybe t
genericFromData pd = G.to <$> hush
  (fromDataWithSchema (Proxy :: Proxy t) (Proxy :: Proxy rep) pd)

-- | Base FromData instances

instance FromData Void where
  fromData _ = Nothing

instance FromData Unit where
  fromData (Constr n [])
    | n == BigNum.zero = Just unit
  fromData _ = Nothing

-- NOTE: For the sake of compatibility the following fromDatas have to match
-- https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-tx/src/PlutusTx/IsData/Instances.hs
instance FromData Boolean where
  fromData (Constr n [])
    | n == BigNum.zero = Just false
    | n == BigNum.one = Just true
  fromData _ = Nothing

instance FromData a => FromData (Maybe a) where
  fromData (Constr n [ pd ])
    | n == BigNum.zero = maybe Nothing (Just <<< Just) (fromData pd) -- Just is zero-indexed by Plutus
  fromData (Constr n [])
    | n == BigNum.one = Just Nothing
  fromData _ = Nothing

instance (FromData a, FromData b) => FromData (Either a b) where
  fromData (Constr n [ pd ])
    | n == BigNum.zero = maybe Nothing (Just <<< Left) (fromData pd)
    | n == BigNum.one = maybe Nothing (Just <<< Right) (fromData pd)
  fromData _ = Nothing

instance Fail (Text "Int is not supported, use BigInt instead") => FromData Int where
  fromData _ = Nothing

instance FromData BigInt where
  fromData (Integer n) = Just n
  fromData _ = Nothing

instance FromData BigNum where
  fromData (Integer n) = BigNum.fromBigInt n
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
    | n == BigNum.zero = Tuple <$> fromData a <*> fromData b
  fromData _ = Nothing

instance FromData ByteArray where
  fromData (Bytes res) = Just res
  fromData _ = Nothing

instance FromData CborBytes where
  fromData = map wrap <<< fromData

instance FromData RawBytes where
  fromData = map wrap <<< fromData

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

instance FromData Uint8Array where
  fromData x = (fromData x :: Maybe ByteArray) <#> unwrap

-- Instances for purescript-noble-secp256k1 types

instance FromData Secp256k1PrivateKey where
  fromData = map wrap <<< mkPrivateKey <=< fromData

instance FromData MessageHash where
  fromData = mkMessageHash <=< fromData

instance FromData ECDSAPublicKey where
  fromData = mkECDSAPublicKey <=< fromData

instance FromData ECDSASignature where
  fromData = map wrap <<< fromData

instance FromData SchnorrPublicKey where
  fromData = mkSchnorrPublicKey <=< fromData

instance FromData SchnorrSignature where
  fromData = map wrap <<< fromData

fromDataUnfoldable
  :: forall (a :: Type) (t :: Type -> Type)
   . Unfoldable t
  => FromData a
  => PlutusData
  -> Maybe (t a)
fromDataUnfoldable (List entries) = Array.toUnfoldable <$> traverse fromData
  entries
fromDataUnfoldable _ = Nothing
