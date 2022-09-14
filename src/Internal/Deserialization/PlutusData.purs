module CTL.Internal.Deserialization.PlutusData
  ( convertPlutusData
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import CTL.Internal.Deserialization.BigInt (convertBigInt)
import CTL.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import CTL.Internal.Serialization.Types
  ( BigInt
  , ConstrPlutusData
  , PlutusData
  , PlutusList
  , PlutusMap
  )
import CTL.Internal.Types.BigNum (BigNum)
import CTL.Internal.Types.BigNum (toBigInt) as BigNum
import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.PlutusData (PlutusData(Constr, Map, List, Integer, Bytes)) as T

convertPlutusData :: PlutusData -> Maybe T.PlutusData
convertPlutusData pd =
  convertPlutusConstr pd
    <|> convertPlutusMap pd
    <|> convertPlutusList pd
    <|> convertPlutusInteger pd
    <|> convertPlutusBytes pd

convertPlutusConstr :: PlutusData -> Maybe T.PlutusData
convertPlutusConstr pd = do
  constr <- _PlutusData_constr maybeFfiHelper pd
  data' <- traverse convertPlutusData
    $ _unpackPlutusList containerHelper
    $ _ConstrPlutusData_data constr
  alt <- BigNum.toBigInt $ _ConstrPlutusData_alternative constr
  pure $ T.Constr alt data'

convertPlutusMap :: PlutusData -> Maybe T.PlutusData
convertPlutusMap pd = do
  entries <- _PlutusData_map maybeFfiHelper pd >>=
    _unpackPlutusMap containerHelper Tuple >>> traverse
      \(k /\ v) -> do
        k' <- convertPlutusData k
        v' <- convertPlutusData v
        pure (k' /\ v')
  pure $ T.Map entries

convertPlutusList :: PlutusData -> Maybe T.PlutusData
convertPlutusList pd = T.List <$> do
  _PlutusData_list maybeFfiHelper pd >>=
    _unpackPlutusList containerHelper >>>
      traverse convertPlutusData

convertPlutusInteger :: PlutusData -> Maybe T.PlutusData
convertPlutusInteger pd = T.Integer <$> do
  _PlutusData_integer maybeFfiHelper pd >>= convertBigInt

convertPlutusBytes :: PlutusData -> Maybe T.PlutusData
convertPlutusBytes pd = T.Bytes <$> _PlutusData_bytes maybeFfiHelper pd

foreign import _PlutusData_constr
  :: MaybeFfiHelper -> PlutusData -> Maybe ConstrPlutusData

foreign import _PlutusData_map
  :: MaybeFfiHelper -> PlutusData -> Maybe PlutusMap

foreign import _PlutusData_list
  :: MaybeFfiHelper -> PlutusData -> Maybe PlutusList

foreign import _PlutusData_integer
  :: MaybeFfiHelper -> PlutusData -> Maybe BigInt

foreign import _PlutusData_bytes
  :: MaybeFfiHelper -> PlutusData -> Maybe ByteArray

foreign import _unpackPlutusList
  :: ContainerHelper -> PlutusList -> Array PlutusData

foreign import _ConstrPlutusData_alternative :: ConstrPlutusData -> BigNum
foreign import _ConstrPlutusData_data :: ConstrPlutusData -> PlutusList
foreign import _unpackPlutusMap
  :: ContainerHelper
  -> (forall a b. a -> b -> Tuple a b)
  -> PlutusMap
  -> Array (PlutusData /\ PlutusData)
