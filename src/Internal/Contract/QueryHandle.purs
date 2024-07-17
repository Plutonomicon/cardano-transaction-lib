module Ctl.Internal.Contract.QueryHandle
  ( queryHandleForCtlBackend
  , queryHandleForBlockfrostBackend
  , queryHandleForSelfHostedBlockfrostBackend
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Log (logDebug')
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend, CtlBackend)
import Ctl.Internal.Contract.QueryHandle.Type (QueryHandle)
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios, getChainTip, submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as QueryM
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries) as QueryM
import Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getOutputAddressesByTxHash
  , getScriptByHash
  , getTxAuxiliaryData
  , getUtxoByOref
  , isTxConfirmed
  , utxosAt
  ) as Kupo
import Ctl.Internal.QueryM.Ogmios (SubmitTxR(SubmitFail, SubmitTxSuccess))
import Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  ) as QueryM
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Ctl.Internal.Service.Error (ClientError(ClientOtherError))
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe, isJust)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
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
  , getTxAuxiliaryData: runQueryM' <<< Kupo.getTxAuxiliaryData
  , utxosAt: runQueryM' <<< Kupo.utxosAt
  , getChainTip: Right <$> runQueryM' QueryM.getChainTip
  , getCurrentEpoch: runQueryM' QueryM.getCurrentEpoch
  , submitTx: \tx -> runQueryM' do
      let txHash = Transaction.hash tx
      logDebug' $ "Pre-calculated tx hash: " <> show txHash
      let txCborBytes = encodeCbor tx
      result <- QueryM.submitTxOgmios txHash txCborBytes
      pure $ case result of
        SubmitTxSuccess th -> do
          if th == txHash then Right th
          else Left
            ( ClientOtherError
                "Computed TransactionHash is not equal to the one returned by Ogmios, please report as bug!"
            )
        SubmitFail err -> Left $ ClientOtherError $ show err
  , evaluateTx: \tx additionalUtxos -> runQueryM' do
      let txBytes = encodeCbor tx
      QueryM.evaluateTxOgmios txBytes additionalUtxos
  , getEraSummaries: Right <$> runQueryM' QueryM.getEraSummaries
  , getPoolIds: Right <$> runQueryM' QueryM.getPoolIds
  , getPubKeyHashDelegationsAndRewards: \_ pubKeyHash ->
      Right <$> runQueryM'
        (QueryM.getPubKeyHashDelegationsAndRewards pubKeyHash)
  , getValidatorHashDelegationsAndRewards: \_ validatorHash ->
      Right <$> runQueryM'
        (QueryM.getValidatorHashDelegationsAndRewards $ wrap validatorHash)
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
  , getTxAuxiliaryData: runBlockfrostServiceM' <<< Blockfrost.getTxAuxiliaryData
  , utxosAt: runBlockfrostServiceM' <<< Blockfrost.utxosAt
  , getChainTip: runBlockfrostServiceM' Blockfrost.getChainTip
  , getCurrentEpoch:
      runBlockfrostServiceM' Blockfrost.getCurrentEpoch >>= case _ of
        Right epoch -> pure $ wrap epoch
        Left err -> throwError $ error $ show err
  , submitTx: runBlockfrostServiceM' <<< Blockfrost.submitTx
  , evaluateTx: \tx additionalUtxos ->
      runBlockfrostServiceM' $ Blockfrost.evaluateTx tx additionalUtxos
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
            (wrap stakeValidatorHash)
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
