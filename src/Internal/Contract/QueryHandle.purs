module Ctl.Internal.Contract.QueryHandle where

import Prelude

import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Contract.Monad
  ( Contract
  , ContractEnv
  , runQueryM
  )
import Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , defaultBackend
  )
import Ctl.Internal.QueryM (ClientError, QueryM)
import Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getDatumsByHashes
  , getScriptByHash
  , getScriptsByHashes
  , getUtxoByOref
  , isTxConfirmed
  , utxosAt
  ) as Kupo
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Undefined (undefined)

type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe Datum)
  , getDatumsByHashes :: Array DataHash -> AffE (Map DataHash Datum)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getScriptsByHashes :: Array ScriptHash -> AffE (Map ScriptHash ScriptRef)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , isTxConfirmed :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle =
  ask <#> \contractEnv ->
    case defaultBackend contractEnv.backend of
      CtlBackend backend ->
        queryHandleForCtlBackend contractEnv backend
      BlockfrostBackend backend ->
        queryHandleForBlockfrostBackend contractEnv backend

queryHandleForCtlBackend :: ContractEnv -> CtlBackend -> QueryHandle
queryHandleForCtlBackend contractEnv backend =
  { getDatumByHash: runQueryM' <<< Kupo.getDatumByHash
  , getDatumsByHashes: runQueryM' <<< Kupo.getDatumsByHashes
  , getScriptByHash: runQueryM' <<< Kupo.getScriptByHash
  , getScriptsByHashes: runQueryM' <<< Kupo.getScriptsByHashes
  , getUtxoByOref: runQueryM' <<< Kupo.getUtxoByOref
  , isTxConfirmed: runQueryM' <<< Kupo.isTxConfirmed
  , utxosAt: runQueryM' <<< Kupo.utxosAt
  }
  where
  runQueryM' :: forall (a :: Type). QueryM a -> Aff a
  runQueryM' = runQueryM contractEnv backend

queryHandleForBlockfrostBackend
  :: ContractEnv -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend = undefined

