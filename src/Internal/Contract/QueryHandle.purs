module Ctl.Internal.Contract.QueryHandle
  ( getQueryHandle
  , QueryHandle
  , AffE
  ) where

import Prelude

import Contract.Log (logDebug', logWarn')
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction
  ( PoolPubKeyHash
  , Transaction
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
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios, getChainTip, submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch) as QueryM
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries) as QueryM
import Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getOutputAddressesByTxHash
  , getScriptByHash
  , getTxMetadata
  , getUtxoByOref
  , isTxConfirmed
  , utxosAt
  ) as Kupo
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , CurrentEpoch
  , SubmitTxR(SubmitFail, SubmitTxSuccess)
  , TxEvaluationR
  )
import Ctl.Internal.QueryM.Pools (DelegationsAndRewards)
import Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  ) as QueryM
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Ctl.Internal.Service.Error (ClientError(ClientOtherError))
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.EraSummaries (EraSummaries)
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Data.Either (Either(Left, Right))
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)

type AffE (a :: Type) = Aff (Either ClientError a)

type QueryHandle =
  { getDatumByHash :: DataHash -> AffE (Maybe Datum)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getTxMetadata ::
      TransactionHash
      -> Aff (Either GetTxMetadataError GeneralTransactionMetadata)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , getOutputAddressesByTxHash :: TransactionHash -> AffE (Array Address)
  , doesTxExist :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , getChainTip :: AffE Chain.Tip
  , getCurrentEpoch :: Aff CurrentEpoch
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> AffE TransactionHash
  , evaluateTx :: Transaction -> AdditionalUtxoSet -> Aff TxEvaluationR
  , getEraSummaries :: AffE EraSummaries
  , getPoolIds :: AffE (Array PoolPubKeyHash)
  , getPubKeyHashDelegationsAndRewards ::
      NetworkId -> StakePubKeyHash -> AffE (Maybe DelegationsAndRewards)
  , getValidatorHashDelegationsAndRewards ::
      NetworkId -> StakeValidatorHash -> AffE (Maybe DelegationsAndRewards)
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle = ask <#> \contractEnv ->
  case contractEnv.backend of
    CtlBackend backend _ ->
      queryHandleForCtlBackend contractEnv backend
    BlockfrostBackend backend _ -> do
      queryHandleForBlockfrostBackend contractEnv backend

queryHandleForCtlBackend :: ContractEnv -> CtlBackend -> QueryHandle
queryHandleForCtlBackend contractEnv backend =
  { getDatumByHash: runQueryM' <<< Kupo.getDatumByHash
  , getScriptByHash: runQueryM' <<< Kupo.getScriptByHash
  , getUtxoByOref: runQueryM' <<< Kupo.getUtxoByOref
  , getOutputAddressesByTxHash: runQueryM' <<< Kupo.getOutputAddressesByTxHash
  , doesTxExist: runQueryM' <<< map (map isJust) <<< Kupo.isTxConfirmed
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
  runQueryM' = runQueryM contractEnv backend

queryHandleForBlockfrostBackend
  :: ContractEnv -> BlockfrostBackend -> QueryHandle
queryHandleForBlockfrostBackend contractEnv backend =
  { getDatumByHash: runBlockfrostServiceM' <<< Blockfrost.getDatumByHash
  , getScriptByHash: runBlockfrostServiceM' <<< Blockfrost.getScriptByHash
  , getUtxoByOref: runBlockfrostServiceM' <<< Blockfrost.getUtxoByOref
  , getOutputAddressesByTxHash: runBlockfrostServiceM' <<<
      Blockfrost.getOutputAddressesByTxHash
  , doesTxExist: runBlockfrostServiceM' <<< Blockfrost.doesTxExist
  , getTxMetadata: runBlockfrostServiceM' <<< Blockfrost.getTxMetadata
  , utxosAt: runBlockfrostServiceM' <<< Blockfrost.utxosAt
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
    (fromMaybe logWithLevel contractEnv.customLogger contractEnv.logLevel)
    backend
