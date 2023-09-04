module Ctl.Internal.Contract.QueryHandle
  ( queryHandleForCtlBackend
  , queryHandleForBlockfrostBackend
  , queryHandleForSelfHostedBlockfrostBackend
  ) where

import Prelude

import Contract.Log (logDebug', logWarn')
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend, CtlBackend)
import Ctl.Internal.Contract.QueryHandle.Type (QueryHandle)
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios, getChainTip, submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as QueryM
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries) as QueryM
import Ctl.Internal.QueryM.Kupo (getDatumByHash, getOutputAddressesByTxHash, getScriptByHash, getTxMetadata, getUtxoByOref, isTxConfirmed, utxosAt, utxosAtScriptHash) as Kupo
import Ctl.Internal.QueryM.Ogmios (SubmitTxR(SubmitFail, SubmitTxSuccess))
import Ctl.Internal.QueryM.Pools (getPoolIds, getPubKeyHashDelegationsAndRewards, getValidatorHashDelegationsAndRewards) as QueryM
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Service.Blockfrost (BlockfrostServiceM, runBlockfrostServiceM)
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Ctl.Internal.Service.Error (ClientError(ClientOtherError))
import Data.Either (Either(Left, Right))
import Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)

queryHandleForCtlBackend
  :: forall rest
   . (forall (a :: Type). LogParams rest -> CtlBackend -> QueryM a -> Aff a)
  -> LogParams rest
  -> CtlBackend
  -> QueryHandle
queryHandleForCtlBackend runQueryM params backend =
  { getDatumByHash: runQueryM' <<< Kupo.getDatumByHash
  , getScriptByHash: runQueryM' <<< Kupo.getScriptByHash
  , getUtxoByOref: runQueryM' <<< Kupo.getUtxoByOref
  , getOutputAddressesByTxHash: runQueryM' <<< Kupo.getOutputAddressesByTxHash
  , doesTxExist: runQueryM' <<< map (map isJust) <<< Kupo.isTxConfirmed
  , getTxMetadata: runQueryM' <<< Kupo.getTxMetadata
  , utxosAt: runQueryM' <<< Kupo.utxosAt
  , utxosAtScriptHash: runQueryM' <<< Kupo.utxosAtScriptHash
  , getChainTip: Right <$> runQueryM' QueryM.getChainTip
  , getCurrentEpoch: runQueryM' QueryM.getCurrentEpoch
  , submitTx: \tx -> runQueryM' do
      cslTx <- liftEffect $ Serialization.convertTransaction tx
      let txHash = Hashing.transactionHash cslTx
      logDebug' $ "Pre-calculated tx hash: " <> show txHash
      let txCborBytes = Serialization.toBytes cslTx
      result <- QueryM.submitTxOgmios (unwrap txHash) txCborBytes
      case result of
        SubmitTxSuccess a -> pure $ pure $ wrap a
        SubmitFail err -> pure $ Left $ ClientOtherError $ show err
  , evaluateTx: \tx additionalUtxos -> runQueryM' do
      txBytes <- Serialization.toBytes <$> liftEffect
        (Serialization.convertTransaction tx)
      QueryM.evaluateTxOgmios txBytes additionalUtxos
  , getEraSummaries: Right <$> runQueryM' QueryM.getEraSummaries
  , getPoolIds: Right <$> runQueryM' QueryM.getPoolIds
  , getPubKeyHashDelegationsAndRewards: \_ pubKeyHash ->
      Right <$> runQueryM'
        (QueryM.getPubKeyHashDelegationsAndRewards pubKeyHash)
  , getValidatorHashDelegationsAndRewards: \_ validatorHash ->
      Right <$> runQueryM'
        (QueryM.getValidatorHashDelegationsAndRewards validatorHash)
  }

  where
  runQueryM' :: forall (a :: Type). QueryM a -> Aff a
  runQueryM' = runQueryM params backend

queryHandleForBlockfrostBackend
  :: forall rest. LogParams rest -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend logParams backend =
  { getDatumByHash: runBlockfrostServiceM' <<< Blockfrost.getDatumByHash
  , getScriptByHash: runBlockfrostServiceM' <<< Blockfrost.getScriptByHash
  , getUtxoByOref: runBlockfrostServiceM' <<< Blockfrost.getUtxoByOref
  , getOutputAddressesByTxHash: runBlockfrostServiceM' <<<
      Blockfrost.getOutputAddressesByTxHash
  , doesTxExist: runBlockfrostServiceM' <<< Blockfrost.doesTxExist
  , getTxMetadata: runBlockfrostServiceM' <<< Blockfrost.getTxMetadata
  , utxosAt: runBlockfrostServiceM' <<< Blockfrost.utxosAt
  , utxosAtScriptHash: runBlockfrostServiceM' <<< Blockfrost.utxosAtScriptHash
  , getChainTip: runBlockfrostServiceM' Blockfrost.getChainTip
  , getCurrentEpoch:
      runBlockfrostServiceM' Blockfrost.getCurrentEpoch >>= case _ of
        Right epoch -> pure $ wrap epoch
        Left err -> throwError $ error $ show err
  , submitTx: runBlockfrostServiceM' <<< Blockfrost.submitTx
  , evaluateTx: \tx additionalUtxos -> runBlockfrostServiceM' do
      unless (Map.isEmpty $ unwrap additionalUtxos) do
        logWarn' "Blockfrost does not support explicit additional utxos"
      Blockfrost.evaluateTx tx
  , getEraSummaries: runBlockfrostServiceM' Blockfrost.getEraSummaries
  , getPoolIds: runBlockfrostServiceM' Blockfrost.getPoolIds
  , getPubKeyHashDelegationsAndRewards: \networkId stakePubKeyHash ->
      runBlockfrostServiceM'
        ( Blockfrost.getPubKeyHashDelegationsAndRewards networkId
            stakePubKeyHash
        )
  , getValidatorHashDelegationsAndRewards: \networkId stakeValidatorHash ->
      runBlockfrostServiceM'
        ( Blockfrost.getValidatorHashDelegationsAndRewards networkId
            stakeValidatorHash
        )
  }
  where
  runBlockfrostServiceM' :: forall (a :: Type). BlockfrostServiceM a -> Aff a
  runBlockfrostServiceM' = runBlockfrostServiceM
    (fromMaybe logWithLevel logParams.customLogger logParams.logLevel)
    backend

queryHandleForSelfHostedBlockfrostBackend
  :: forall rest
   . LogParams rest
  -> BlockfrostBackend
  -> (forall (a :: Type). LogParams rest -> CtlBackend -> QueryM a -> Aff a)
  -> CtlBackend
  -> QueryHandle
queryHandleForSelfHostedBlockfrostBackend
  params
  blockfrostBackend
  runQueryM
  ctlBackend =
  let
    blockfrostQueryHandle = queryHandleForBlockfrostBackend params
      blockfrostBackend
    ctlQueryHandle = queryHandleForCtlBackend runQueryM params ctlBackend
  in
    blockfrostQueryHandle
      { evaluateTx = ctlQueryHandle.evaluateTx
      , submitTx = ctlQueryHandle.submitTx
      }
