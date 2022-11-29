module Ctl.Internal.Contract.QueryHandle where

import Prelude

import Contract.Log (logDebug')
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , UtxoMap
  )
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
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.QueryM (ClientError, QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios, getChainTip, submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as QueryM
import Ctl.Internal.QueryM.GetTxByHash (getTxByHash) as QueryM
import Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getDatumsByHashes
  , getScriptByHash
  , getScriptsByHashes
  , getUtxoByOref
  , isTxConfirmed
  , utxosAt
  ) as Kupo
import Ctl.Internal.QueryM.Ogmios (AdditionalUtxoSet, CurrentEpoch) as Ogmios
import Ctl.Internal.QueryM.Ogmios (SubmitTxR(SubmitTxSuccess), TxEvaluationR)
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Undefined (undefined)
import Untagged.Union (asOneOf)

-- Why ClientError?
type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe Datum)
  , getDatumsByHashes :: Array DataHash -> AffE (Map DataHash Datum)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getScriptsByHashes :: Array ScriptHash -> AffE (Map ScriptHash ScriptRef)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , isTxConfirmed :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , getChainTip :: Aff Chain.Tip
  , getCurrentEpoch :: Aff Ogmios.CurrentEpoch
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> Aff (Maybe TransactionHash)
  , getTxByHash :: TransactionHash -> Aff (Maybe Transaction)
  , evaluateTx :: Transaction -> Ogmios.AdditionalUtxoSet -> Aff TxEvaluationR

  -- getTxByHash
  --   bf: /txs/{hash}
  --   ctl: requires ODC
  --     Not a problem for now
  -- submitTx
  -- evaluateTx
  -- chainTip
  -- getProtocolParameters
  -- this gets done early on
  -- perhaps genesis/systemStart should be too
  -- getConstantParameters
  -- systemStart
  -- currentEpoch
  -- we need era summaries start + end, and the era summaries slot length
  -- ogmios has eraSummaries, BF has epochs for start + end, and genesis for slot length (idk if this is safe)
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle =
  ask <#> \contractEnv ->
    case defaultBackend contractEnv.backend of
      CtlBackend backend ->
        queryHandleForCtlBackend contractEnv backend
      BlockfrostBackend backend ->
        queryHandleForBlockfrostBackend contractEnv backend

getQueryHandle' :: ContractEnv -> QueryHandle
getQueryHandle' contractEnv =
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
  , getChainTip: runQueryM' QueryM.getChainTip
  , getCurrentEpoch: runQueryM' QueryM.getCurrentEpoch
  , submitTx: \tx -> runQueryM' do
      cslTx <- liftEffect $ Serialization.convertTransaction tx
      let txHash = Hashing.transactionHash cslTx
      logDebug' $ "Pre-calculated tx hash: " <> show txHash
      let txCborBytes = wrap $ Serialization.toBytes $ asOneOf cslTx
      result <- QueryM.submitTxOgmios (unwrap txHash) txCborBytes
      case result of
        SubmitTxSuccess a -> pure $ Just $ wrap a
        _ -> pure Nothing
  , getTxByHash: runQueryM' <<< QueryM.getTxByHash <<< unwrap
  , evaluateTx: \tx additionalUtxos -> runQueryM' do
      txBytes <- liftEffect
        ( wrap <<< Serialization.toBytes <<< asOneOf <$>
            Serialization.convertTransaction tx
        )
      QueryM.evaluateTxOgmios txBytes additionalUtxos
  }
  where
  runQueryM' :: forall (a :: Type). QueryM a -> Aff a
  runQueryM' = runQueryM contractEnv backend

queryHandleForBlockfrostBackend
  :: ContractEnv -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend = undefined

