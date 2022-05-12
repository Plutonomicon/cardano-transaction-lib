-- | This module that defines query functionality via Ogmios to get `PlutusData`
-- | from `DatumHash` along with related `PlutusData` newtype wrappers such as
-- | `Datum` and `Redeemer`. It also contains typeclasses like `FromData` and
-- | `ToData`.
module Contract.PlutusData
  ( cancelFetchBlocks
  , datumHash
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocks
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
import QueryM (DatumCacheListeners, DatumCacheWebSocket, defaultDatumCacheWsConfig, mkDatumCacheWebSocketAff) as ExportQueryM
import QueryM (cancelFetchBlocks, datumHash, getDatumByHash, getDatumsByHashes, startFetchBlocks) as QueryM
import Serialization.Address (Slot)
import ToData (class ToData, toData) as ToData
import Types.Datum (Datum(Datum), DatumHash, unitDatum) as Datum
import Types.PlutusData (PlutusData(Constr, Map, List, Integer, Bytes)) as PlutusData
import Types.Redeemer (Redeemer(Redeemer), RedeemerHash(RedeemerHash), redeemerHash, unitRedeemer) as Redeemer
import Types.Transaction (BlockId, DatumHash)
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

startFetchBlocks
  :: forall (r :: Row Type)
   . { slot :: Slot, id :: BlockId }
  -> Contract r Unit
startFetchBlocks = wrapContract <<< QueryM.startFetchBlocks

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocks :: forall (r :: Row Type). Contract r Unit
cancelFetchBlocks = wrapContract QueryM.cancelFetchBlocks

-- | Hashes a Plutus-style Datum
datumHash :: forall (r :: Row Type). Datum.Datum -> Contract r (Maybe DatumHash)
datumHash = wrapContract <<< QueryM.datumHash
