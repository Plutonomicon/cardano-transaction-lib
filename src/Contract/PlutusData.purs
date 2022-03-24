-- | This module that defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash` along with related `PlutusData` newtype wrappers such as
-- | `Datum` and `Redeemer`. It also contains typeclasses like `FromData` and
-- | `ToData`.
module Contract.PlutusData
  ( cancelFetchBlocksRequest
  , datumFilterAddHashesRequest
  , datumFilterGetHashesRequest
  , datumFilterRemoveHashesRequest
  , datumFilterSetHashesRequest
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocksRequest
  , module Datum
  , module ExportQueryM
  , module PlutusData
  , module Redeemer
  , module FromData
  , module ToData
  , module Transaction
  ) where

import Prelude
import Contract.Monad (Contract)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import FromData (class FromData, fromData) as FromData
import QueryM
  ( cancelFetchBlocksRequest
  , datumFilterAddHashesRequest
  , datumFilterGetHashesRequest
  , datumFilterRemoveHashesRequest
  , datumFilterSetHashesRequest
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocksRequest
  ) as QueryM
import QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , defaultDatumCacheWsConfig
  , mkDatumCacheWebSocketAff
  ) as ExportQueryM
import Serialization.Address (Slot, BlockId)
import ToData (class ToData, toData) as ToData
import Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) as PlutusData
import Types.Datum
  ( Datum(Datum)
  , DatumHash
  , unitDatum
  , datumHash
  ) as Datum
import Types.Redeemer
  ( Redeemer(Redeemer)
  , RedeemerHash(RedeemerHash)
  , redeemerHash
  , unitRedeemer
  ) as Redeemer
-- Not importing `RedeemerTag` for now.
import Types.Transaction (DatumHash)
import Types.Transaction (DataHash(DataHash)) as Transaction

-- | Get a `PlutusData` given a `DatumHash`.
getDatumByHash :: DatumHash -> Contract (Maybe PlutusData.PlutusData)
getDatumByHash = wrap <<< QueryM.getDatumByHash

-- | Get `PlutusData`s given a an `Array` of `DatumHash`.
getDatumsByHashes :: Array DatumHash -> Contract (Array PlutusData.PlutusData)
getDatumsByHashes = wrap <<< QueryM.getDatumsByHashes

startFetchBlocksRequest :: { slot :: Slot, id :: BlockId } -> Contract Unit
startFetchBlocksRequest = wrap <<< QueryM.startFetchBlocksRequest

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocksRequest :: Contract Unit
cancelFetchBlocksRequest = wrap QueryM.cancelFetchBlocksRequest

datumFilterAddHashesRequest :: Array DatumHash -> Contract Unit
datumFilterAddHashesRequest = wrap <<< QueryM.datumFilterAddHashesRequest

datumFilterRemoveHashesRequest :: Array DatumHash -> Contract Unit
datumFilterRemoveHashesRequest = wrap <<< QueryM.datumFilterRemoveHashesRequest

datumFilterSetHashesRequest :: Array DatumHash -> Contract Unit
datumFilterSetHashesRequest = wrap <<< QueryM.datumFilterSetHashesRequest

datumFilterGetHashesRequest :: Contract (Array DatumHash)
datumFilterGetHashesRequest = wrap QueryM.datumFilterGetHashesRequest
