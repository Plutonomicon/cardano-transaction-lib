-- | This module that defines query functionality to get `PlutusData`
-- | from `DatumHash`.
module Contract.PlutusData
  ( getDatumByHash
  , getDatumsByHashes
  , getDatumsByHashesWithErrors
  ) where

import Prelude

import Cardano.Types (DataHash, PlutusData)
import Contract.Monad (Contract)
import Control.Parallel (parTraverse)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Data.Either (Either(Left, Right), hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (liftAff)

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
