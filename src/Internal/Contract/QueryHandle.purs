module Ctl.Internal.Contract.QueryHandle where

import Prelude

import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Contract.Monad
  ( BlockfrostBackend
  , Contract
  , ContractEnv
  , CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , runQueryM
  )
import Ctl.Internal.QueryM (ClientError)
import Ctl.Internal.QueryM.Kupo (utxosAt) as Kupo
import Ctl.Internal.Serialization.Address (Address)
import Data.Either (Either)
import Effect.Aff (Aff)
import Undefined (undefined)

type QueryHandle =
  { utxosAt :: Address -> Aff (Either ClientError UtxoMap)
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle =
  ask <#> \contractEnv ->
    case contractEnv.backend of
      CtlBackend backend ->
        queryHandleForCtlBackend contractEnv backend
      BlockfrostBackend backend ->
        queryHandleForBlockfrostBackend contractEnv backend

queryHandleForCtlBackend :: ContractEnv -> CtlBackend -> QueryHandle
queryHandleForCtlBackend contractEnv backend =
  { utxosAt:
      runQueryM contractEnv backend <<< Kupo.utxosAt
  }

queryHandleForBlockfrostBackend
  :: ContractEnv -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend = undefined

