module Ctl.Internal.Contract.Provider
  ( providerForCtlBackend
  , providerForBlockfrostBackend
  , providerForSelfHostedBlockfrostBackend
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Blockfrost.BlockfrostBackend (BlockfrostBackend)
import Cardano.Blockfrost.Service
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Cardano.Blockfrost.Service as Blockfrost
import Cardano.Provider.Error (ClientError(ClientOtherError))
import Cardano.Provider.Type (Provider)
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Log (logDebug')
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.ProviderBackend (CtlBackend)
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
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Exception (error)

providerForCtlBackend
  :: forall rest
   . (forall (a :: Type). LogParams rest -> CtlBackend -> QueryM a -> Aff a)
  -> LogParams rest
  -> CtlBackend
  -> Provider
providerForCtlBackend runQueryM params backend =
  { getDatumByHash: runQueryM' <<< Kupo.getDatumByHash
  , getScriptByHash: runQueryM' <<< Kupo.getScriptByHash
  , getUtxoByOref: runQueryM' <<< Kupo.getUtxoByOref
  , getOutputAddressesByTxHash: runQueryM' <<< Kupo.getOutputAddressesByTxHash
  , doesTxExist: runQueryM' <<< map (map isJust) <<< Kupo.isTxConfirmed
  , getTxAuxiliaryData: runQueryM' <<< Kupo.getTxAuxiliaryData
  , utxosAt: runQueryM' <<< Kupo.utxosAt
  , getChainTip: Right <$> runQueryM' QueryM.getChainTip
  , getCurrentEpoch: unwrap <$> runQueryM' QueryM.getCurrentEpoch
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
  , evaluateTx: \tx additionalUtxos -> unwrap <$> runQueryM' do
      let txBytes = encodeCbor tx
      QueryM.evaluateTxOgmios txBytes (wrap additionalUtxos)
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

providerForBlockfrostBackend
  :: forall rest. LogParams rest -> BlockfrostBackend -> Provider
providerForBlockfrostBackend logParams backend =
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
        Right epoch -> pure epoch
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

providerForSelfHostedBlockfrostBackend
  :: forall rest
   . LogParams rest
  -> BlockfrostBackend
  -> (forall (a :: Type). LogParams rest -> CtlBackend -> QueryM a -> Aff a)
  -> CtlBackend
  -> Provider
providerForSelfHostedBlockfrostBackend
  params
  blockfrostBackend
  runQueryM
  ctlBackend =
  let
    blockfrostProvider = providerForBlockfrostBackend params
      blockfrostBackend
    ctlProvider = providerForCtlBackend runQueryM params ctlBackend
  in
    blockfrostProvider
      { evaluateTx = ctlProvider.evaluateTx
      , submitTx = ctlProvider.submitTx
      }
