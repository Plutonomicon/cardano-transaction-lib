-- | This module that defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash` along with related `PlutusData` newtype wrappers such as
-- | `Datum` and `Redeemer`. It also contains typeclasses like `FromData` and
-- | `ToData` plus everything related to `PlutusSchema`.
module Contract.PlutusData
  ( getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  , module DataSchema
  , module Datum
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
import Control.Parallel (parTraverse)
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
import Data.Either (Either(Left, Right), hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (liftAff)

-- | Retrieve the full resolved datum associated to a given datum hash.
getDatumByHash :: DataHash -> Contract (Maybe Datum)
getDatumByHash dataHash = do
  queryHandle <- getQueryHandle
  liftAff $ join <<< hush <$> queryHandle.getDatumByHash dataHash

-- | Retrieve full resolved datums associated with given datum hashes.
-- | The resulting `Map` will only contain datums that have been successfully
-- | resolved.
getDatumsByHashes :: Array DataHash -> Contract (Map DataHash Datum)
getDatumsByHashes hashes =
  Map.mapMaybe hush <$> getDatumsByHashesWithErrors hashes

-- | Retrieve full resolved datums associated with given datum hashes.
-- | Errors are returned per datum.
getDatumsByHashesWithErrors
  :: Array DataHash -> Contract (Map DataHash (Either String Datum))
getDatumsByHashesWithErrors hashes = do
  queryHandle <- getQueryHandle
  liftAff $ Map.fromFoldable <$> flip parTraverse hashes
    \dh -> queryHandle.getDatumByHash dh <#> Tuple dh <<< case _ of
      Right (Just datum) -> Right datum
      Right Nothing -> Left "Datum not found"
      Left err -> Left $ show err
