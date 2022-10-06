module Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except)
import Control.Monad.Trans.Class (lift)
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
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (evaluateTxOgmios) as QueryM
import Ctl.Internal.QueryM.MinFee (calculateMinFee) as QueryM
import Ctl.Internal.QueryM.Ogmios (TxEvaluationResult(TxEvaluationResult)) as Ogmios
import Ctl.Internal.ReindexRedeemers
  ( ReindexErrors
  , reindexSpentScriptRedeemers'
  )
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Transaction (setScriptDataHash)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Natural (toBigInt) as Natural
import Ctl.Internal.Types.ScriptLookups
  ( UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  )
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.UnbalancedTransaction (_transaction)
import Data.Array (catMaybes)
import Data.Array (findIndex, fromFoldable, uncons) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Index (ix) as Lens
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Map (empty, fromFoldable, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Untagged.Union (asOneOf)

evalTxExecutionUnits
  :: Transaction
  -> UnattachedUnbalancedTx
  -> BalanceTxM Ogmios.TxEvaluationResult
evalTxExecutionUnits tx unattachedTx = do
  txBytes <- liftEffect
    ( wrap <<< Serialization.toBytes <<< asOneOf <$>
        Serialization.convertTransaction tx
    )
  eResult <- unwrap <$> lift (QueryM.evaluateTxOgmios txBytes)
  case eResult of
    Right a -> pure a
    Left e | tx ^. _isValid -> throwError $ ExUnitsEvaluationFailed unattachedTx
      e
    Left _ -> pure $ wrap $ Map.empty

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
  reindexedUnattachedTx <-
    ExceptT $ reindexRedeemers unattachedTx <#> lmap ReindexRedeemersError
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
  minFee <- ExceptT $ QueryM.calculateMinFee finalizedTx <#> pure <<< unwrap
  pure $ reindexedUnattachedTxWithExUnits /\ minFee

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
setRdmrsExecutionUnits rs (Ogmios.TxEvaluationResult xxs) =
  case Array.uncons (Map.toUnfoldable xxs) of
    Nothing -> rs
    Just { head: ptr /\ exUnits, tail: xs } ->
      let
        xsWrapped = Ogmios.TxEvaluationResult (Map.fromFoldable xs)
        ixMaybe = flip Array.findIndex rs $ \(Redeemer rdmr /\ _) ->
          rdmr.tag == ptr.redeemerTag
            && rdmr.index == Natural.toBigInt ptr.redeemerIndex
      in
        ixMaybe # maybe (setRdmrsExecutionUnits rs xsWrapped) \ix ->
          flip setRdmrsExecutionUnits xsWrapped $
            rs # Lens.ix ix %~ \(Redeemer rec /\ txOutRef) ->
              let
                mem = Natural.toBigInt exUnits.memory
                steps = Natural.toBigInt exUnits.steps
              in
                Redeemer rec { exUnits = { mem, steps } } /\ txOutRef
