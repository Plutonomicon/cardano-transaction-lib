module Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import Contract.Numeric.Natural (fromInt') as Natural
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)
import Ctl.Internal.BalanceTx.Constraints
  ( _additionalUtxos
  , _collateralUtxos
  ) as Constraints
import Ctl.Internal.BalanceTx.Error
  ( BalanceTxError(UtxoLookupFailedFor, ExUnitsEvaluationFailed)
  )
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( IndexedRedeemer
  , attachRedeemers
  , indexedRedeemerToRedeemer
  )
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftContract
  )
import Ctl.Internal.BalanceTx.UnattachedTx (EvaluatedTx, IndexedTx)
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
  , _isValid
  , _plutusData
  , _witnessSet
  )
import Ctl.Internal.Contract.MinFee (calculateMinFee) as Contract.MinFee
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap)
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , TxEvaluationFailure(AdditionalUtxoOverlap)
  , TxEvaluationResult(TxEvaluationResult)
  ) as Ogmios
import Ctl.Internal.QueryM.Ogmios (TxEvaluationFailure(UnparsedError))
import Ctl.Internal.Transaction (setScriptDataHash)
import Ctl.Internal.TxOutput
  ( transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  )
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Natural (toBigInt) as Natural
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (catMaybes)
import Data.Array (fromFoldable, notElem) as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((?~))
import Data.Map (empty, filterKeys, fromFoldable, lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import JS.BigInt (BigInt)

evalTxExecutionUnits
  :: Transaction
  -> BalanceTxM Ogmios.TxEvaluationResult
evalTxExecutionUnits tx = do
  networkId <- askNetworkId
  additionalUtxos <-
    fromPlutusUtxoMap networkId <$> asksConstraints Constraints._additionalUtxos
  worker $ toOgmiosAdditionalUtxos additionalUtxos
  where
  toOgmiosAdditionalUtxos :: UtxoMap -> Ogmios.AdditionalUtxoSet
  toOgmiosAdditionalUtxos additionalUtxos =
    wrap $ Map.fromFoldable
      ( bimap transactionInputToTxOutRef transactionOutputToOgmiosTxOut
          <$> (Map.toUnfoldable :: _ -> Array _) additionalUtxos
      )

  worker :: Ogmios.AdditionalUtxoSet -> BalanceTxM Ogmios.TxEvaluationResult
  worker additionalUtxos = do
    queryHandle <- liftContract getQueryHandle
    evalResult <-
      unwrap <$> liftContract
        (liftAff $ queryHandle.evaluateTx tx additionalUtxos)
    case evalResult of
      Right a -> pure a
      Left (Ogmios.AdditionalUtxoOverlap overlappingUtxos) ->
        -- Remove overlapping additional utxos and retry evaluation:
        worker $ wrap $ Map.filterKeys (flip Array.notElem overlappingUtxos)
          (unwrap additionalUtxos)
      Left evalFailure | tx ^. _isValid ->
        throwError $ ExUnitsEvaluationFailed tx evalFailure
      Left _ ->
        pure $ wrap Map.empty

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnbalancedTx` and the minimum fee.
evalExUnitsAndMinFee
  :: IndexedTx
  -> UtxoMap
  -> BalanceTxM (EvaluatedTx /\ BigInt)
evalExUnitsAndMinFee unattachedTx allUtxos = do
  -- Reattach datums and redeemers before evaluating ex units:
  let attachedTx = reattachDatumsAndFakeRedeemers unattachedTx
  -- Evaluate transaction ex units:
  exUnits <- evalTxExecutionUnits attachedTx
  -- Set execution units received from the server:
  txWithExUnits <-
    case updateTxExecutionUnits unattachedTx exUnits of
      Just res -> pure res
      Nothing
        | not (attachedTx ^. _isValid) -> pure $
            unattachedTx
              { redeemers = indexedRedeemerToRedeemer <$> unattachedTx.redeemers
              }
      _ -> throwError $ ExUnitsEvaluationFailed attachedTx
        (UnparsedError "Unable to extract ExUnits from Ogmios response")
  -- Attach datums and redeemers, set the script integrity hash:
  FinalizedTransaction finalizedTx <- finalizeTransaction txWithExUnits allUtxos
  -- Calculate the minimum fee for a transaction:
  networkId <- askNetworkId
  additionalUtxos <-
    fromPlutusUtxoMap networkId
      <$> asksConstraints Constraints._additionalUtxos
  collateralUtxos <-
    fromPlutusUtxoMap networkId <<< fromMaybe Map.empty
      <$> asksConstraints Constraints._collateralUtxos
  minFee <- liftContract $ Contract.MinFee.calculateMinFee finalizedTx
    (Map.union additionalUtxos collateralUtxos)
  pure $ txWithExUnits /\ unwrap minFee

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

updateTxExecutionUnits
  :: IndexedTx
  -> Ogmios.TxEvaluationResult
  -> Maybe EvaluatedTx
updateTxExecutionUnits tx@{ redeemers } result =
  getRedeemersExUnits result redeemers <#> \redeemers' -> tx
    { redeemers = redeemers' }

getRedeemersExUnits
  :: Ogmios.TxEvaluationResult
  -> Array IndexedRedeemer
  -> Maybe (Array Redeemer)
getRedeemersExUnits (Ogmios.TxEvaluationResult result) redeemers = do
  for redeemers \indexedRedeemer -> do
    { memory, steps } <- Map.lookup
      { redeemerTag: (unwrap indexedRedeemer).tag
      , redeemerIndex: Natural.fromInt' (unwrap indexedRedeemer).index
      }
      result
    pure $ Redeemer $ (unwrap $ indexedRedeemerToRedeemer indexedRedeemer)
      { exUnits =
          { mem: Natural.toBigInt memory
          , steps: Natural.toBigInt steps
          }
      }
