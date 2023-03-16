module Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except.Trans (except)
import Ctl.Internal.BalanceTx.Constraints (_additionalUtxos) as Constraints
import Ctl.Internal.BalanceTx.Error (BalanceTxError(..))
import Ctl.Internal.BalanceTx.Helpers (_body')
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( IndexedRedeemer(..)
  , attachRedeemers
  , indexRedeemers
  , indexedRedeemerToRedeemer
  , mkRedeemersContext
  )
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftContract
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
import Ctl.Internal.Contract.MinFee (calculateMinFee) as Contract.MinFee
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap)
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , TxEvaluationResult(TxEvaluationResult)
  ) as Ogmios
import Ctl.Internal.ReindexRedeemers
  ( ReindexErrors
  , reindexSpentScriptRedeemers'
  )
import Ctl.Internal.Serialization.Types (Redeemers)
import Ctl.Internal.Transaction (setScriptDataHash)
import Ctl.Internal.TxOutput
  ( transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  )
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Natural (fromBigInt', toBigInt) as Natural
import Ctl.Internal.Types.ScriptLookups
  ( EvaluatedTx
  , IndexedTx
  , PrebalancedTx(..)
  , UnattachedUnbalancedIndexedEvaluatedTx(..)
  , UnattachedUnbalancedIndexedTx(UnattachedUnbalancedIndexedTx)
  , UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  , UnindexedTx
  )
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.UnbalancedTransaction (UnbalancedTx(..), _transaction)
import Data.Array (catMaybes)
import Data.Array (fromFoldable) as Array
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), note)
import Data.Foldable (fold, foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Map (empty, fromFoldable, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Undefined (undefined)

evalTxExecutionUnits
  :: Transaction
  -> BalanceTxM Ogmios.TxEvaluationResult
evalTxExecutionUnits tx = do
  queryHandle <- liftContract getQueryHandle
  additionalUtxos <- getOgmiosAdditionalUtxoSet
  evalResult <-
    unwrap <$> liftContract
      (liftAff $ queryHandle.evaluateTx tx additionalUtxos)

  case evalResult of
    Right a -> pure a
    Left evalFailure | tx ^. _isValid ->
      throwError $ TODO
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
  :: PrebalancedTx
  -> UtxoMap
  -> BalanceTxM (EvaluatedTx /\ BigInt)
evalExUnitsAndMinFee (PrebalancedTx unattachedTx) allUtxos = do
  -- Reattach datums and redeemers before evaluating ex units:
  let attachedTx = reattachDatumsAndFakeRedeemers unattachedTx
  -- Evaluate transaction ex units:
  exUnits <- evalTxExecutionUnits attachedTx
  -- Set execution units received from the server:
  reindexedUnattachedTxWithExUnits <- liftMaybe TODO $
    updateTxExecutionUnits' unattachedTx exUnits
  -- Attach datums and redeemers, set the script integrity hash:
  FinalizedTransaction finalizedTx <-
    finalizeTransaction reindexedUnattachedTxWithExUnits allUtxos
  -- Calculate the minimum fee for a transaction:
  networkId <- askNetworkId
  additionalUtxos <-
    fromPlutusUtxoMap networkId
      <$> asksConstraints Constraints._additionalUtxos
  minFee <- liftContract $ Contract.MinFee.calculateMinFee finalizedTx
    additionalUtxos
  pure $ reindexedUnattachedTxWithExUnits /\ unwrap minFee

-- | Attaches datums and redeemers, sets the script integrity hash,
-- | for use after reindexing.
finalizeTransaction
  :: EvaluatedTx -> UtxoMap -> BalanceTxM FinalizedTransaction
finalizeTransaction tx utxos = do
  let
    attachedTxWithExUnits :: Transaction
    attachedTxWithExUnits =
      reattachDatumsAndRedeemers tx

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

-- reindexRedeemers'
--   :: UnindexedTx
--   -> Maybe IndexedTx
-- reindexRedeemers' { transaction, redeemers, datums } = do
--   indexedRedeemers <- indexRedeemers ctx redeemers
--   pure { datums, redeemers: indexedRedeemers, transaction }
--   where
--   ctx = mkRedeemersContext transaction

-- reindexRedeemers
--   :: UnattachedUnbalancedTx
--   -> Either ReindexErrors UnattachedUnbalancedTx
-- reindexRedeemers
--   unattachedTx@(UnattachedUnbalancedTx { redeemers }) =
--   let
--     inputs = Array.fromFoldable $ unattachedTx ^. _body' <<< _inputs
--   in
--     reindexSpentScriptRedeemers' inputs redeemersTxIns <#>
--       \redeemersTxInsReindexed ->
--         unattachedTx # _redeemers .~ redeemersTxInsReindexed

reattachDatumsAndFakeRedeemers :: IndexedTx -> Transaction
reattachDatumsAndFakeRedeemers
  { transaction, datums, redeemers } =
  reattachDatumsAndRedeemers
    { transaction, datums, redeemers: indexedRedeemerToRedeemer <$> redeemers }

reattachDatumsAndRedeemers :: EvaluatedTx -> Transaction
reattachDatumsAndRedeemers
  ({ transaction, datums, redeemers }) =
  let
    transaction' = attachRedeemers redeemers transaction
  in
    transaction'
      # _witnessSet <<< _plutusData ?~ map unwrap datums

-- updateTxExecutionUnits
--   :: UnattachedUnbalancedTx
--   -> Ogmios.TxEvaluationResult
--   -> UnattachedUnbalancedTx
-- updateTxExecutionUnits unattachedTx rdmrPtrExUnitsList =
--   unattachedTx
--     # _redeemersTxIns %~ flip setRdmrsExecutionUnits rdmrPtrExUnitsList

updateTxExecutionUnits'
  :: IndexedTx
  -> Ogmios.TxEvaluationResult
  -> Maybe EvaluatedTx
updateTxExecutionUnits' tx@{ redeemers } result =
  getRedeemersExUnits result redeemers >>= \redeemers' -> pure $ tx
    { redeemers = redeemers' }

getRedeemersExUnits
  :: Ogmios.TxEvaluationResult
  -> Array IndexedRedeemer
  -> Maybe (Array Redeemer)
getRedeemersExUnits (Ogmios.TxEvaluationResult result) redeemers = do
  for redeemers \indexedRedeemer -> do
    let
      Redeemer redeemer = indexedRedeemerToRedeemer indexedRedeemer
      index = BigInt.fromInt (unwrap indexedRedeemer).index
    { memory, steps } <- Map.lookup
      { redeemerTag: redeemer.tag
      , redeemerIndex: Natural.fromBigInt' index
      }
      result
    pure $ Redeemer redeemer
      { exUnits =
          { mem: Natural.toBigInt memory
          , steps: Natural.toBigInt steps
          }
      }

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
