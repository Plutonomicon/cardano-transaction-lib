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
  , module Serialization
  , module Deserialization
  , module Redeemer
  , module FromData
  , module ToData
  , module OutputDatum
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Ctl.Internal.Deserialization.PlutusData (deserializeData) as Deserialization
import Ctl.Internal.FromData
  ( class FromData
  , class FromDataArgs
  , class FromDataArgsRL
  , class FromDataWithSchema
  , FromDataError
      ( ArgsWantedButGot
      , FromDataFailed
      , BigIntToIntFailed
      , IndexWantedButGot
      , WantedConstrGot
      )
  , fromData
  , fromDataArgs
  , fromDataArgsRec
  , fromDataWithSchema
  , genericFromData
  ) as FromData
import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.IsData (class IsData) as IsData
import Ctl.Internal.Plutus.Types.DataSchema
  ( class AllUnique2
  , class HasPlutusSchema
  , class PlutusSchemaToRowListI
  , class SchemaToRowList
  , class ValidPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , ApPCons
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
  ) as DataSchema
import Ctl.Internal.QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , defaultDatumCacheWsConfig
  , mkDatumCacheWebSocketAff
  ) as ExportQueryM
import Ctl.Internal.QueryM
  ( getDatumByHash
  , getDatumsByHashes
  ) as QueryM
import Ctl.Internal.Serialization (serializeData) as Serialization
import Ctl.Internal.ToData
  ( class ToData
  , class ToDataArgs
  , class ToDataArgsRL
  , class ToDataArgsRLHelper
  , class ToDataWithSchema
  , genericToData
  , toData
  , toDataArgs
  , toDataArgsRec
  , toDataArgsRec'
  , toDataWithSchema
  ) as ToData
import Ctl.Internal.Types.Datum (DataHash)
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum(Datum), unitDatum) as Datum
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  ) as OutputDatum
import Ctl.Internal.Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as PlutusData
import Ctl.Internal.Types.Redeemer
  ( Redeemer(Redeemer)
  , RedeemerHash(RedeemerHash)
  , redeemerHash
  , unitRedeemer
  ) as Redeemer
import Data.Map (Map)
import Data.Maybe (Maybe)

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
