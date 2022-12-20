module Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)
import Ctl.Internal.BalanceTx.Constraints (_additionalUtxos) as Constraints
import Ctl.Internal.BalanceTx.Error
  ( BalanceTxError
      ( ExUnitsEvaluationFailed
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      )
  )
import Ctl.Internal.BalanceTx.Helpers (_body', _redeemersTxIns)
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftEitherQueryM
  , liftQueryM
  )
import Ctl.Internal.Cardano.Types.ScriptRef as ScriptRef
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls
  , Redeemer(Redeemer)
  , Transaction
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet
  , TxBody(TxBody)
  , UtxoMap
  , _body
  , _inputs
  , _isValid
  , _plutusData
  , _redeemers
  , _witnessSet
  )
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios) as QueryM
import Ctl.Internal.QueryM.MinFee (calculateMinFee) as QueryM
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , TxEvaluationResult(TxEvaluationResult)
  ) as Ogmios
import Ctl.Internal.ReindexRedeemers
  ( ReindexErrors
  , reindexSpentScriptRedeemers'
  )
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Transaction (setScriptDataHash)
import Ctl.Internal.TxOutput
  ( transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  )
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Natural (fromBigInt', toBigInt) as Natural
import Ctl.Internal.Types.ScriptLookups
  ( UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  )
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.UnbalancedTransaction (_transaction)
import Data.Array (catMaybes)
import Data.Array (fromFoldable) as Array
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Map (empty, fromFoldable, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)

evalTxExecutionUnits
  :: Transaction
  -> UnattachedUnbalancedTx
  -> BalanceTxM Ogmios.TxEvaluationResult
evalTxExecutionUnits tx unattachedTx = do
  txBytes <- liftEffect
    $ Serialization.toBytes <$> Serialization.convertTransaction tx
  additionalUtxos <- getOgmiosAdditionalUtxoSet
  evalResult <-
    unwrap <$> liftQueryM (QueryM.evaluateTxOgmios txBytes additionalUtxos)

  case evalResult of
    Right a -> pure a
    Left evalFailure | tx ^. _isValid ->
      throwError $ ExUnitsEvaluationFailed unattachedTx evalFailure
    Left _ -> pure $ wrap Map.empty
  where
  getOgmiosAdditionalUtxoSet :: BalanceTxM Ogmios.AdditionalUtxoSet
  getOgmiosAdditionalUtxoSet = do
    networkId <- askNetworkId
    additionalUtxos <-
      asksConstraints Constraints._additionalUtxos
        <#> fromPlutusUtxoMap networkId
    pure $ wrap $ Map.fromFoldable
      ( bimap transactionInputToTxOutRef transactionOutputToOgmiosTxOut
          <$> (Map.toUnfoldable :: _ -> Array _) additionalUtxos
      )

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnattachedUnbalancedTx` and
-- the minimum fee.
evalExUnitsAndMinFee
  :: PrebalancedTransaction
  -> UtxoMap
  -> BalanceTxM (UnattachedUnbalancedTx /\ BigInt)
evalExUnitsAndMinFee (PrebalancedTransaction unattachedTx) allUtxos = do
  -- Reindex `Spent` script redeemers:
  reindexedUnattachedTx <- liftEitherQueryM $
    reindexRedeemers unattachedTx <#> lmap ReindexRedeemersError
  -- Reattach datums and redeemers before evaluating ex units:
  let attachedTx = reattachDatumsAndRedeemers reindexedUnattachedTx
  -- Evaluate transaction ex units:
  rdmrPtrExUnitsList <- evalTxExecutionUnits attachedTx reindexedUnattachedTx
  let
    -- Set execution units received from the server:
    reindexedUnattachedTxWithExUnits =
      updateTxExecutionUnits reindexedUnattachedTx rdmrPtrExUnitsList
  -- Attach datums and redeemers, set the script integrity hash:
  FinalizedTransaction finalizedTx <-
    finalizeTransaction reindexedUnattachedTxWithExUnits allUtxos
  -- Calculate the minimum fee for a transaction:
  networkId <- askNetworkId
  additionalUtxos <-
    fromPlutusUtxoMap networkId
      <$> asksConstraints Constraints._additionalUtxos
  minFee <- liftQueryM $ QueryM.calculateMinFee finalizedTx additionalUtxos
  pure $ reindexedUnattachedTxWithExUnits /\ unwrap minFee

-- | Attaches datums and redeemers, sets the script integrity hash,
-- | for use after reindexing.
finalizeTransaction
  :: UnattachedUnbalancedTx -> UtxoMap -> BalanceTxM FinalizedTransaction
finalizeTransaction reindexedUnattachedTxWithExUnits utxos = do
  let
    attachedTxWithExUnits :: Transaction
    attachedTxWithExUnits =
      reattachDatumsAndRedeemers reindexedUnattachedTxWithExUnits

    txBody :: TxBody
    txBody = attachedTxWithExUnits ^. _body

    ws :: TransactionWitnessSet
    ws = attachedTxWithExUnits ^. _witnessSet

    redeemers :: Array Redeemer
    redeemers = fromMaybe mempty (_.redeemers $ unwrap ws)

    datums :: Array Datum
    datums = wrap <$> fromMaybe mempty (_.plutusData $ unwrap ws)

  refPlutusScripts <- except $ getRefPlutusScripts txBody

  let
    scripts :: Array PlutusScript
    scripts = fromMaybe mempty (_.plutusScripts $ unwrap ws) <> refPlutusScripts

    languages :: Set Language
    languages = foldMap (Set.singleton <<< snd <<< unwrap) scripts

  (costModels :: Costmdls) <- askCostModelsForLanguages languages

  liftEffect $ FinalizedTransaction <$>
    setScriptDataHash costModels redeemers datums attachedTxWithExUnits
  where
  getRefPlutusScripts :: TxBody -> Either BalanceTxError (Array PlutusScript)
  getRefPlutusScripts (TxBody txBody) =
    let
      spendAndRefInputs :: Array TransactionInput
      spendAndRefInputs =
        Array.fromFoldable (txBody.inputs <> txBody.referenceInputs)
    in
      catMaybes <<< map getPlutusScript <$>
        for spendAndRefInputs \oref ->
          note (UtxoLookupFailedFor oref) (Map.lookup oref utxos)

  getPlutusScript :: TransactionOutput -> Maybe PlutusScript
  getPlutusScript (TransactionOutput { scriptRef }) =
    ScriptRef.getPlutusScript =<< scriptRef

reindexRedeemers
  :: UnattachedUnbalancedTx
  -> QueryM (Either ReindexErrors UnattachedUnbalancedTx)
reindexRedeemers
  unattachedTx@(UnattachedUnbalancedTx { redeemersTxIns }) =
  let
    inputs = Array.fromFoldable $ unattachedTx ^. _body' <<< _inputs
  in
    reindexSpentScriptRedeemers' inputs redeemersTxIns <#>
      map \redeemersTxInsReindexed ->
        unattachedTx # _redeemersTxIns .~ redeemersTxInsReindexed

reattachDatumsAndRedeemers :: UnattachedUnbalancedTx -> Transaction
reattachDatumsAndRedeemers
  (UnattachedUnbalancedTx { unbalancedTx, datums, redeemersTxIns }) =
  let
    transaction = unbalancedTx ^. _transaction
  in
    transaction # _witnessSet <<< _plutusData ?~ map unwrap datums
      # _witnessSet <<< _redeemers ?~ map fst redeemersTxIns

updateTxExecutionUnits
  :: UnattachedUnbalancedTx
  -> Ogmios.TxEvaluationResult
  -> UnattachedUnbalancedTx
updateTxExecutionUnits unattachedTx rdmrPtrExUnitsList =
  unattachedTx #
    _redeemersTxIns %~ flip setRdmrsExecutionUnits rdmrPtrExUnitsList

setRdmrsExecutionUnits
  :: Array (Redeemer /\ Maybe TransactionInput)
  -> Ogmios.TxEvaluationResult
  -> Array (Redeemer /\ Maybe TransactionInput)
setRdmrsExecutionUnits rs (Ogmios.TxEvaluationResult evalR) =
  rs <#> \r@(Redeemer rec@{ tag: redeemerTag, index } /\ oref) ->
    Map.lookup { redeemerTag, redeemerIndex: Natural.fromBigInt' index } evalR
      # maybe r \{ memory, steps } ->
          Redeemer rec
            { exUnits =
                { mem: Natural.toBigInt memory, steps: Natural.toBigInt steps }
            } /\ oref
