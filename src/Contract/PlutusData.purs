-- | This module that defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash` along with related `PlutusData` newtype wrappers such as
-- | `Datum` and `Redeemer`. It also contains typeclasses like `FromData` and
-- | `ToData`.
module Contract.PlutusData
  ( getDatumByHash
  , getDatumsByHashes
  , module DataSchema
  , module Datum
  , module ExportQueryM
  , module Hashing
  , module IsData
  , module PlutusData
  , module Redeemer
  , module FromData
  , module ToData
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Data.Map (Map)
import Data.Maybe (Maybe)
import FromData
  ( FromDataError
      ( ArgsWantedButGot
      , FromDataFailed
      , BigIntToIntFailed
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
  ) as FromData
import Hashing (datumHash) as Hashing
import Plutus.Types.DataSchema
  ( ApPCons
  , Field
  , I
  , Id
  , IxK
  , MkField
  , MkField_
  , MkIxK
  , MkIxK_
  , PCons
  , PNil
  , PSchema
  , class AllUnique2
  , class HasPlutusSchema
  , class PlutusSchemaToRowListI
  , class SchemaToRowList
  , class ValidPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  ) as DataSchema
import QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , defaultDatumCacheWsConfig
  , mkDatumCacheWebSocketAff
  ) as ExportQueryM
import QueryM
  ( getDatumByHash
  , getDatumsByHashes
  ) as QueryM
import ToData
  ( class ToData
  , class ToDataArgs
  , class ToDataWithSchema
  , class ToDataArgsRL
  , class ToDataArgsRLHelper
  , genericToData
  , toDataArgsRec
  , toDataArgsRec'
  , toData
  , toDataArgs
  , toDataWithSchema
  ) as ToData
import Types.Datum (DataHash(DataHash), Datum(Datum), unitDatum) as Datum
import Types.Datum (DataHash)
import Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as PlutusData
import Types.Redeemer
  ( Redeemer(Redeemer)
  , RedeemerHash(RedeemerHash)
  , redeemerHash
  , unitRedeemer
  ) as Redeemer
import IsData (class IsData) as IsData

-- | Get a `PlutusData` given a `DatumHash`.
getDatumByHash
  :: forall (r :: Row Type)
   . DataHash
  -> Contract r (Maybe Datum.Datum)
getDatumByHash = wrapContract <<< QueryM.getDatumByHash

-- | Get `PlutusData`s given a an `Array` of `DataHash`.
getDatumsByHashes
  :: forall (r :: Row Type)
   . Array DataHash
  -> Contract r (Map DataHash Datum.Datum)
getDatumsByHashes = wrapContract <<< QueryM.getDatumsByHashes
