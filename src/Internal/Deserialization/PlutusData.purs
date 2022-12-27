module Ctl.Internal.Deserialization.PlutusData
  ( convertPlutusData
  , deserializeData
  ) where

import Prelude

import Ctl.Internal.Deserialization.BigInt (convertBigInt)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , containerHelper
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Serialization.Types
  ( BigInt
  , ConstrPlutusData
  , PlutusData
  , PlutusList
  , PlutusMap
  )
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as T
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

type ConvertPlutusData =
  { constr :: ConstrPlutusData -> T.PlutusData
  , map :: PlutusMap -> T.PlutusData
  , list :: PlutusList -> T.PlutusData
  , integer :: BigInt -> T.PlutusData
  , bytes :: ByteArray -> T.PlutusData
  }

convertPlutusData :: PlutusData -> T.PlutusData
convertPlutusData pd = _convertPlutusData
  { constr: convertPlutusConstr
  , map: convertPlutusMap
  , list: convertPlutusList
  , integer: convertPlutusInteger
  , bytes: convertPlutusBytes
  }
  pd

convertPlutusConstr :: ConstrPlutusData -> T.PlutusData
convertPlutusConstr constr = do
  let
    data' = convertPlutusData <$>
      _unpackPlutusList containerHelper (_ConstrPlutusData_data constr)
    alt = _ConstrPlutusData_alternative constr
  T.Constr alt data'

convertPlutusMap :: PlutusMap -> T.PlutusData
convertPlutusMap =
  _unpackPlutusMap containerHelper Tuple
    >>> map
      (\(k /\ v) -> (convertPlutusData k /\ convertPlutusData v))
    >>> T.Map

convertPlutusList :: PlutusList -> T.PlutusData
convertPlutusList =
  _unpackPlutusList containerHelper >>> map (\d -> convertPlutusData d) >>>
    T.List

-- Unsafe fromJust here is correct, due to arbitrary sized integers
convertPlutusInteger :: BigInt -> T.PlutusData
convertPlutusInteger i = T.Integer $ unsafePartial $ fromJust $ convertBigInt i

convertPlutusBytes :: ByteArray -> T.PlutusData
convertPlutusBytes = T.Bytes

deserializeData :: forall (a :: Type). FromData a => CborBytes -> Maybe a
deserializeData = fromData <<< convertPlutusData <=< fromBytes

foreign import _convertPlutusData
  :: ConvertPlutusData -> PlutusData -> T.PlutusData

foreign import _unpackPlutusList
  :: ContainerHelper -> PlutusList -> Array PlutusData

foreign import _ConstrPlutusData_alternative :: ConstrPlutusData -> BigNum
foreign import _ConstrPlutusData_data :: ConstrPlutusData -> PlutusList
foreign import _unpackPlutusMap
  :: ContainerHelper
  -> (forall a b. a -> b -> Tuple a b)
  -> PlutusMap
  -> Array (PlutusData /\ PlutusData)
