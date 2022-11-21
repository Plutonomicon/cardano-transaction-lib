module Ctl.Internal.QueryM.QueryHandle where

import Prelude

import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.QueryM
  ( ClientError
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryM
  )
import Ctl.Internal.QueryM.Blockfrost (getDatumByHash, utxosAt) as Blockfrost
import Ctl.Internal.QueryM.Kupo (getDatumByHash, utxosAt) as Kupo
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)

type QueryME (a :: Type) = QueryM (Either ClientError a)

type QueryHandle =
  { utxosAt :: Address -> QueryME UtxoMap
  , getDatumByHash :: DataHash -> QueryME (Maybe Datum)
  -- , getDatumByHashes :: Array DataHash -> QueryME (Map DataHash Datum)
  -- , getScriptByHash :: ScriptHash -> QueryME (Maybe ScriptRef)
  -- , getScriptsByHashes :: Array ScriptHash -> QueryME (Map ScriptHash ScriptRef)
  }

getQueryHandle :: QueryM QueryHandle
getQueryHandle =
  asks (_.backend <<< _.config) <#> case _ of
    CtlBackend _ ->
      { utxosAt: Kupo.utxosAt
      , getDatumByHash: Kupo.getDatumByHash
      }
    BlockfrostBackend _ ->
      { utxosAt: Blockfrost.utxosAt
      , getDatumByHash: Blockfrost.getDatumByHash
      }

