module Ctl.Internal.Contract.QueryHandle
  ( getQueryHandle
  , QueryHandle
  , AffE
  ) where

import Prelude

import Contract.Log (logDebug')
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , UtxoMap
  )
import Ctl.Internal.Contract.Monad (Contract, ContractEnv, runQueryM)
import Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  )
import Ctl.Internal.Contract.QueryHandle.Error (GetTxMetadataError)
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios, getChainTip, submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as QueryM
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries) as QueryM
import Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getScriptByHash
  , getTxMetadata
  , getUtxoByOref
  , isTxConfirmed
  , utxosAt
  ) as Kupo
import Ctl.Internal.QueryM.Ogmios (AdditionalUtxoSet, CurrentEpoch) as Ogmios
import Ctl.Internal.QueryM.Ogmios (SubmitTxR(SubmitTxSuccess), TxEvaluationR)
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.EraSummaries (EraSummaries)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Undefined (undefined)

type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe Datum)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getTxMetadata ::
      TransactionHash
      -> Aff (Either GetTxMetadataError GeneralTransactionMetadata)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , isTxConfirmed :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , getChainTip :: AffE Chain.Tip
  , getCurrentEpoch :: Aff Ogmios.CurrentEpoch
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> Aff (Maybe TransactionHash)
  , evaluateTx :: Transaction -> Ogmios.AdditionalUtxoSet -> Aff TxEvaluationR
  , getEraSummaries :: AffE EraSummaries
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle = ask <#> \contractEnv ->
  case contractEnv.backend of
    CtlBackend backend _ ->
      queryHandleForCtlBackend contractEnv backend
    BlockfrostBackend backend _ ->
      queryHandleForBlockfrostBackend contractEnv backend

queryHandleForCtlBackend :: ContractEnv -> CtlBackend -> QueryHandle
queryHandleForCtlBackend contractEnv backend =
  { getDatumByHash: runQueryM' <<< Kupo.getDatumByHash
  , getScriptByHash: runQueryM' <<< Kupo.getScriptByHash
  , getUtxoByOref: runQueryM' <<< Kupo.getUtxoByOref
  , isTxConfirmed: runQueryM' <<< map (map isJust) <<< Kupo.isTxConfirmed
  , getTxMetadata: runQueryM' <<< Kupo.getTxMetadata
  , utxosAt: runQueryM' <<< Kupo.utxosAt
  , getChainTip: Right <$> runQueryM' QueryM.getChainTip
  , getCurrentEpoch: runQueryM' QueryM.getCurrentEpoch
  , submitTx: \tx -> runQueryM' do
      cslTx <- liftEffect $ Serialization.convertTransaction tx
      let txHash = Hashing.transactionHash cslTx
      logDebug' $ "Pre-calculated tx hash: " <> show txHash
      let txCborBytes = Serialization.toBytes cslTx
      result <- QueryM.submitTxOgmios (unwrap txHash) txCborBytes
      case result of
        SubmitTxSuccess a -> pure $ Just $ wrap a
        _ -> pure Nothing
  , evaluateTx: \tx additionalUtxos -> runQueryM' do
      txBytes <- Serialization.toBytes <$> liftEffect
        (Serialization.convertTransaction tx)
      QueryM.evaluateTxOgmios txBytes additionalUtxos
  , getEraSummaries: Right <$> runQueryM' QueryM.getEraSummaries
  }
  where
  runQueryM' :: forall (a :: Type). QueryM a -> Aff a
  runQueryM' = runQueryM contractEnv backend

queryHandleForBlockfrostBackend
  :: ContractEnv -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend _ backend =
  { getDatumByHash: runBlockfrostServiceM' <<< undefined
  , getScriptByHash: runBlockfrostServiceM' <<< undefined
  , getUtxoByOref: runBlockfrostServiceM' <<< undefined
  , isTxConfirmed: runBlockfrostServiceM' <<< Blockfrost.isTxConfirmed
  , getTxMetadata: runBlockfrostServiceM' <<< Blockfrost.getTxMetadata
  , utxosAt: runBlockfrostServiceM' <<< undefined
  , getChainTip: runBlockfrostServiceM' Blockfrost.getChainTip
  , getCurrentEpoch: runBlockfrostServiceM' undefined
  , submitTx: runBlockfrostServiceM' <<< undefined
  , evaluateTx: \tx additionalUtxos -> runBlockfrostServiceM' $ undefined tx
      additionalUtxos
  , getEraSummaries: runBlockfrostServiceM' Blockfrost.getEraSummaries
  }
  where
  runBlockfrostServiceM' :: forall (a :: Type). BlockfrostServiceM a -> Aff a
  runBlockfrostServiceM' = runBlockfrostServiceM backend
