module Ctl.Internal.Serialization.PlutusData
  ( convertPlutusData
  , packPlutusList
  ) where

import Prelude

import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import Ctl.Internal.Serialization.Types
  ( BigInt
  , ConstrPlutusData
  , PlutusData
  , PlutusList
  , PlutusMap
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.PlutusData as T
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)

convertPlutusData :: T.PlutusData -> PlutusData
convertPlutusData x = case x of
  T.Constr alt list -> convertConstr alt list
  T.Map mp -> convertPlutusMap mp
  T.List lst -> convertPlutusList lst
  T.Integer n -> convertPlutusInteger n
  T.Bytes b -> _mkPlutusData_bytes b

convertConstr :: BigNum -> Array T.PlutusData -> PlutusData
convertConstr alt list =
  _mkPlutusData_constr $ _mkConstrPlutusData
    alt
    (_packPlutusList containerHelper $ map convertPlutusData list)

convertPlutusList :: Array T.PlutusData -> PlutusData
convertPlutusList x =
  (_mkPlutusData_list <<< (_packPlutusList containerHelper)) $
    (map convertPlutusData x)

convertPlutusMap :: Array (T.PlutusData /\ T.PlutusData) -> PlutusData
convertPlutusMap mp =
  let
    entries :: Array (PlutusData /\ PlutusData)
    entries = mp <#> \(k /\ v) -> (convertPlutusData k /\ convertPlutusData v)
  in
    _mkPlutusData_map $ _packMap fst snd entries

convertPlutusInteger :: BigInt.BigInt -> PlutusData
convertPlutusInteger n =
  _mkPlutusData_integer $ convertBigInt n

convertBigInt :: BigInt.BigInt -> BigInt
-- Unsafe is safe here, cuz both BigInt's are dynamic sized,
-- so range errors are not a concern, and `BigInt.toString` always
-- returns parsable string
convertBigInt n = unsafePartial $ fromJust $
  _bigIntFromString maybeFfiHelper (BigInt.toString n)

packPlutusList :: Array T.PlutusData -> PlutusList
packPlutusList = (_packPlutusList containerHelper)
  <<< map convertPlutusData

foreign import _mkPlutusData_bytes :: ByteArray -> PlutusData
foreign import _mkPlutusData_list :: PlutusList -> PlutusData
foreign import _mkPlutusData_map :: PlutusMap -> PlutusData
foreign import _mkPlutusData_integer :: BigInt -> PlutusData
foreign import _mkPlutusData_constr :: ConstrPlutusData -> PlutusData

foreign import _packPlutusList
  :: ContainerHelper -> Array PlutusData -> PlutusList

foreign import _mkConstrPlutusData :: BigNum -> PlutusList -> ConstrPlutusData
foreign import _bigIntFromString :: MaybeFfiHelper -> String -> Maybe BigInt
foreign import _packMap
  :: (forall a b. Tuple a b -> a)
  -> (forall a b. Tuple a b -> b)
  -> Array (PlutusData /\ PlutusData)
  -> PlutusMap
