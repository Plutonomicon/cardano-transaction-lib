-- | This module that defines query functionality to get `PlutusData`
-- | from `DatumHash`.
module Contract.PlutusData
  ( getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  , unitDatum
  , unitRedeemer
  , module X
  , Datum
  , Redeemer
  ) where

import Prelude

import Cardano.FromData
  ( class FromData
  , class FromDataArgs
  , class FromDataArgsRL
  , class FromDataWithSchema
  , FromDataError
      ( ArgsWantedButGot
      , FromDataFailed
      , BigNumToIntFailed
      , IndexWantedButGot
      , WantedConstrGot
      )
  , fromData
  , fromDataArgs
  , fromDataArgsRec
  , fromDataWithSchema
  , genericFromData
  ) as X
import Cardano.Plutus.DataSchema
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
  ) as X
import Cardano.ToData
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
  ) as X
import Cardano.Types (DataHash)
import Cardano.Types
  ( DataHash(DataHash)
  , PlutusData(Constr, Map, List, Integer, Bytes)
  ) as X
import Cardano.Types.OutputDatum
  ( OutputDatum(OutputDatumHash, OutputDatum)
  ) as X
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as Datum
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.RedeemerDatum (RedeemerDatum(RedeemerDatum)) as X
import Cardano.Types.RedeemerDatum as Redeemer
import Contract.Monad (Contract)
import Control.Parallel (parTraverse)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Data.Either (Either(Left, Right), hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (liftAff)
import Prim.TypeError (class Warn, Text)

-- | Retrieve the full resolved datum associated to a given datum hash.
getDatumByHash :: DataHash -> Contract (Maybe PlutusData)
getDatumByHash dataHash = do
  queryHandle <- getQueryHandle
  liftAff $ join <<< hush <$> queryHandle.getDatumByHash dataHash

-- | Retrieve full resolved datums associated with given datum hashes.
-- | The resulting `Map` will only contain datums that have been successfully
-- | resolved.
getDatumsByHashes :: Array DataHash -> Contract (Map DataHash PlutusData)
getDatumsByHashes hashes =
  Map.mapMaybe hush <$> getDatumsByHashesWithErrors hashes

-- | Retrieve full resolved datums associated with given datum hashes.
-- | Errors are returned per datum.
getDatumsByHashesWithErrors
  :: Array DataHash -> Contract (Map DataHash (Either String PlutusData))
getDatumsByHashesWithErrors hashes = do
  queryHandle <- getQueryHandle
  liftAff $ Map.fromFoldable <$> flip parTraverse hashes
    \dh -> queryHandle.getDatumByHash dh <#> Tuple dh <<< case _ of
      Right (Just datum) -> Right datum
      Right Nothing -> Left "Datum not found"
      Left err -> Left $ show err

unitDatum
  :: Warn (Text "Deprecated: unitDatum. use Cardano.Types.PlutusData.unit")
  => PlutusData
unitDatum = Datum.unit

unitRedeemer
  :: Warn
       (Text "Deprecated: unitRedeemer. use Cardano.Types.RedeemerDatum.unit")
  => RedeemerDatum
unitRedeemer = Redeemer.unit

-- | DEPRECATED. Use `Cardano.Types.PlutusData`
type Datum = PlutusData

-- | DEPRECATED. Use `Contract.PlutusData.RedeemerDatum`
type Redeemer = RedeemerDatum
