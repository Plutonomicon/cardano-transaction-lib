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
  , datumHash
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

import Contract.Monad (Contract, wrapContract)
import Data.Map (Map)
import Data.Maybe (Maybe)
import FromData (class FromData, fromData) as FromData
import QueryM
  ( cancelFetchBlocksRequest
  , datumFilterAddHashesRequest
  , datumFilterGetHashesRequest
  , datumFilterRemoveHashesRequest
  , datumFilterSetHashesRequest
  , datumHash
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
getDatumByHash
  :: forall (r :: Row Type)
   . DatumHash
  -> Contract r (Maybe Datum.Datum)
getDatumByHash = wrapContract <<< QueryM.getDatumByHash

-- | Get `PlutusData`s given a an `Array` of `DatumHash`.
getDatumsByHashes
  :: forall (r :: Row Type)
   . Array DatumHash
  -> Contract r (Map DatumHash Datum.Datum)
getDatumsByHashes = wrapContract <<< QueryM.getDatumsByHashes

startFetchBlocksRequest
  :: forall (r :: Row Type)
   . { slot :: Slot, id :: BlockId }
  -> Contract r Unit
startFetchBlocksRequest = wrapContract <<< QueryM.startFetchBlocksRequest

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocksRequest :: forall (r :: Row Type). Contract r Unit
cancelFetchBlocksRequest = wrapContract QueryM.cancelFetchBlocksRequest

datumFilterAddHashesRequest
  :: forall (r :: Row Type). Array DatumHash -> Contract r Unit
datumFilterAddHashesRequest =
  wrapContract <<< QueryM.datumFilterAddHashesRequest

datumFilterRemoveHashesRequest
  :: forall (r :: Row Type). Array DatumHash -> Contract r Unit
datumFilterRemoveHashesRequest =
  wrapContract <<< QueryM.datumFilterRemoveHashesRequest

datumFilterSetHashesRequest
  :: forall (r :: Row Type). Array DatumHash -> Contract r Unit
datumFilterSetHashesRequest =
  wrapContract <<< QueryM.datumFilterSetHashesRequest

datumFilterGetHashesRequest
  :: forall (r :: Row Type). Contract r (Array DatumHash)
datumFilterGetHashesRequest = wrapContract QueryM.datumFilterGetHashesRequest

-- | Hashes a Plutus-style Datum
datumHash :: forall (r :: Row Type). Datum.Datum -> Contract r (Maybe DatumHash)
datumHash = wrapContract <<< QueryM.datumHash
