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
  ) where

import Prelude
import Contract.Monad (Contract)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
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
import Serialization.Address (Slot, BlockId)
import Types.PlutusData (PlutusData)
import Types.Datum
  ( Datum(Datum)
  , DatumHash
  , unitDatum
  , datumHash
  ) as Datum
import Types.Datum
  ( Datum(Datum)
  , DatumHash
  , unitDatum
  , datumHash
  ) as Datum
import Types.Transaction (DatumHash)

-- | This module defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash`.

-- | Get a `PlutusData` given a `DatumHash`.
getDatumByHash :: DatumHash -> Contract (Maybe PlutusData)
getDatumByHash = wrap <<< QueryM.getDatumByHash

-- | Get `PlutusData`s given a an `Array` of `DatumHash`.
getDatumsByHashes :: Array DatumHash -> Contract (Array PlutusData)
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
