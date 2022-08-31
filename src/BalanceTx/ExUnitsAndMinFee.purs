module BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import BalanceTx.Error
  ( BalanceTxError(ExUnitsEvaluationFailed, ReindexRedeemersError)
  )
import BalanceTx.Helpers (_body', _redeemersTxIns)
import BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  )
import Cardano.Types.Transaction
  ( Redeemer(Redeemer)
  , Transaction
  , _inputs
  , _plutusData
  , _redeemers
  , _witnessSet
  )
import Contract.Address (Ed25519KeyHash)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (findIndex, fromFoldable, uncons) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either)
import Data.Lens.Getter ((^.))
import Data.Lens.Index (ix) as Lens
import Data.Lens.Setter ((.~), (?~), (%~))
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (liftEffect)
import QueryM (QueryM)
import QueryM (evaluateTxOgmios) as QueryM
import QueryM.MinFee (calculateMinFee) as QueryM
import QueryM.Ogmios (TxEvaluationResult(TxEvaluationResult)) as Ogmios
import ReindexRedeemers (ReindexErrors, reindexSpentScriptRedeemers')
import Serialization (convertTransaction, toBytes) as Serialization
import Transaction (setScriptDataHash)
import Types.Natural (toBigInt) as Natural
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (_transaction)
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
  ExceptT $ QueryM.evaluateTxOgmios txBytes
    <#> lmap (ExUnitsEvaluationFailed unattachedTx) <<< unwrap

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnattachedUnbalancedTx` and
-- the minimum fee.
evalExUnitsAndMinFee
  :: PrebalancedTransaction -> BalanceTxM (UnattachedUnbalancedTx /\ BigInt)
evalExUnitsAndMinFee (PrebalancedTransaction unattachedTx) = do
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
  FinalizedTransaction finalizedTx <- lift $
    finalizeTransaction reindexedUnattachedTxWithExUnits
  -- Calculate the minimum fee for a transaction:
  minFee <- ExceptT $ QueryM.calculateMinFee finalizedTx <#> pure <<< unwrap
  pure $ reindexedUnattachedTxWithExUnits /\ minFee

-- | Attaches datums and redeemers, sets the script integrity hash,
-- | for use after reindexing.
finalizeTransaction
  :: UnattachedUnbalancedTx -> QueryM FinalizedTransaction
finalizeTransaction reindexedUnattachedTxWithExUnits =
  let
    attachedTxWithExUnits =
      reattachDatumsAndRedeemers reindexedUnattachedTxWithExUnits
    ws = attachedTxWithExUnits ^. _witnessSet # unwrap
    redeemers = fromMaybe mempty ws.redeemers
    datums = wrap <$> fromMaybe mempty ws.plutusData
  in
    do
      costModels <- asks (_.runtime >>> _.pparams >>> unwrap >>> _.costModels)
      liftEffect $ FinalizedTransaction <$>
        setScriptDataHash costModels redeemers datums attachedTxWithExUnits

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

