-- | This module that defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash` along with related `PlutusData` newtype wrappers such as
-- | `Datum` and `Redeemer`. It also contains typeclasses like `FromData` and
-- | `ToData` plus everything related to `PlutusSchema`.
module Contract.PlutusData
  ( getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithError
  , module DataSchema
  , module Datum
  , module ExportQueryM
  , module Hashing
  , module IsData
  , module Nat
  , module PlutusData
  , module Serialization
  , module Deserialization
  , module Redeemer
  , module FromData
  , module ToData
  , module OutputDatum
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
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
import Ctl.Internal.TypeLevel.Nat (Nat, S, Z) as Nat
import Ctl.Internal.Types.Datum (DataHash, Datum)
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
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)

-- | Retrieve the full resolved datum associated to a given datum hash.
getDatumByHash :: DataHash -> Contract (Maybe Datum)
getDatumByHash dataHash = do
  queryHandle <- getQueryHandle
  liftAff $ join <<< hush <$> queryHandle.getDatumByHash dataHash

-- | Retrieve full resolved datums associated with given datum hashes.
-- | The resulting `Map` will only contain datums that have been successfully
-- | resolved. This function returns `Nothing` in case of an error during
-- | response processing (bad HTTP code or response parsing error).
getDatumsByHashes :: Array DataHash -> Contract (Maybe (Map DataHash Datum))
getDatumsByHashes hashes = do
  queryHandle <- getQueryHandle
  liftAff $ hush <$> queryHandle.getDatumsByHashes hashes

-- | Retrieve full resolved datums associated with given datum hashes.
-- | The resulting `Map` will only contain datums that have been successfully
-- | resolved.
getDatumsByHashesWithError
  :: Array DataHash -> Contract (Either String (Map DataHash Datum))
getDatumsByHashesWithError hashes = do
  queryHandle <- getQueryHandle
  liftAff $ lmap show <$> queryHandle.getDatumsByHashes hashes

