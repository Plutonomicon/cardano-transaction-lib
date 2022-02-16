module Serialization.PlutusData
  ( convertPlutusData
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import FfiHelpers (ContainerHelper, MaybeFfiHelper, containerHelper, maybeFfiHelper)
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.Types (BigInt, BigNum, ConstrPlutusData, PlutusData, PlutusList, PlutusMap)
import Types.ByteArray (ByteArray)
import Types.PlutusData as T

convertPlutusData :: T.PlutusData -> Maybe PlutusData
convertPlutusData = case _ of
  T.Constr alt list -> convertConstr alt list
  T.Map mp -> convertPlutusMap mp
  T.List lst -> convertPlutusList lst
  T.Integer n -> convertPlutusInteger n
  T.Bytes b -> pure $ _mkPlutusData_bytes b

convertConstr :: BigInt.BigInt -> Array T.PlutusData -> Maybe PlutusData
convertConstr alt list =
  map _mkPlutusData_constr $ _mkConstrPlutusData
    <$> bigNumFromBigInt alt
    <*> (_packPlutusList containerHelper <$> for list convertPlutusData)

convertPlutusList :: Array T.PlutusData -> Maybe PlutusData
convertPlutusList x =
  _mkPlutusData_list <<< _packPlutusList containerHelper <$> traverse convertPlutusData x

convertPlutusMap :: Map T.PlutusData T.PlutusData -> Maybe PlutusData
convertPlutusMap mp = do
  entries <- for (Data.Map.toUnfoldable mp) \(k /\ v) -> do
    k' <- convertPlutusData k
    v' <- convertPlutusData v
    pure $ k' /\ v'
  _mkPlutusData_map <$> _packMap maybeFfiHelper fst snd entries

convertPlutusInteger :: BigInt.BigInt -> Maybe PlutusData
convertPlutusInteger n =
  _mkPlutusData_integer <$> convertBigInt n

convertBigInt :: BigInt.BigInt -> Maybe BigInt
convertBigInt n = _bigIntFromString maybeFfiHelper (BigInt.toString n)

foreign import _mkPlutusData_bytes :: ByteArray -> PlutusData
foreign import _mkPlutusData_list :: PlutusList -> PlutusData
foreign import _mkPlutusData_map :: PlutusMap -> PlutusData
foreign import _mkPlutusData_integer :: BigInt -> PlutusData
foreign import _mkPlutusData_constr :: ConstrPlutusData -> PlutusData

foreign import _packPlutusList :: ContainerHelper -> Array PlutusData -> PlutusList
foreign import _mkConstrPlutusData :: BigNum -> PlutusList -> ConstrPlutusData
foreign import _bigIntFromString :: MaybeFfiHelper -> String -> Maybe BigInt
foreign import _packMap
  :: MaybeFfiHelper
  -> (forall a b. Tuple a b -> a)
  -> (forall a b. Tuple a b -> b)
  -> Array (PlutusData /\ PlutusData)
  -> Maybe PlutusMap
