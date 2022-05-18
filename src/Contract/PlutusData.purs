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
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
import Data.Map (Map)
import Data.Maybe (Maybe)
import FromData (class FromData, fromData) as FromData
import QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , defaultDatumCacheWsConfig
  , mkDatumCacheWebSocketAff
  ) as ExportQueryM
import QueryM
  ( cancelFetchBlocks
  , datumHash
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocks
  ) as QueryM
import Serialization.Address (Slot)
import ToData (class ToData, toData) as ToData
import Types.Chain (BlockHeaderHash)
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

startFetchBlocks
  :: forall (r :: Row Type)
   . { slot :: Slot, id :: BlockHeaderHash }
  -> Contract r Unit
startFetchBlocks = wrapContract <<< QueryM.startFetchBlocks

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocks :: forall (r :: Row Type). Contract r Unit
cancelFetchBlocks = wrapContract QueryM.cancelFetchBlocks

-- | Hashes a Plutus-style Datum
datumHash :: forall (r :: Row Type). Datum.Datum -> Contract r (Maybe DataHash)
datumHash = wrapContract <<< QueryM.datumHash
