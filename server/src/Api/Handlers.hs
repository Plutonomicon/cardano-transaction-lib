module Api.Handlers (
  estimateTxFees,
  applyArgs,
  evalTxExecutionUnits,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Tx qualified as Tx
import Cardano.Ledger.Alonzo.TxWitness qualified as TxWitness
import Cardano.Ledger.Core qualified as Ledger (TxBody)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value qualified as Value
import Control.Lens ((&), (<&>))
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Kind (Type)
import Data.List qualified as List (find)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text.Encoding
import Data.Traversable (for)
import Math.NumberTheory.Logarithms qualified as Math (integerLog2)
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import Types (
  AppM,
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  CardanoError (
    AcquireFailure,
    EraMismatchError,
    ScriptExecutionError,
    TxValidityIntervalError
  ),
  Cbor (Cbor),
  CborDecodeError (InvalidCbor, InvalidHex),
  CtlServerError (CardanoError, CborDecode),
  Env (protocolParams),
  EvalExUnitsRequest (EvalExUnitsRequest, tx),
  ExecutionUnitsMap (ExecutionUnitsMap),
  Fee (Fee),
  FeesRequest (FeesRequest, count, tx),
  RdmrPtrExUnits (
    RdmrPtrExUnits,
    exUnitsMem,
    exUnitsSteps,
    rdmrPtrIdx,
    rdmrPtrTag
  ),
  WitnessCount (WitnessCount),
  getNodeConnectInfo,
 )

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

estimateTxFees :: FeesRequest -> AppM Fee
estimateTxFees FeesRequest {count, tx} = do
  decodeCborTx tx & either (throwM . CborDecode) pure >>= \case
    C.Tx txBody' keyWits -> do
      pparams <- asks protocolParams
      -- calculate and set script integrity hash before estimating fees
      let WitnessCount witCount = count
          txBody = setScriptIntegrityHash pparams txBody'
          fee = estimateFee pparams witCount (C.Tx txBody keyWits)
      Fee <$> finalizeTxFee fee
  where
    -- `txfee` value must also be taken into account when calculating fees,
    -- since it affects the final transaction size.
    finalizeTxFee :: Integer -> AppM Integer
    finalizeTxFee fee
      -- `integerLog2` would fail on zero,
      -- since logarithm of zero is mathematically undefined
      | fee == 0 = pure 0
      | otherwise =
        asks protocolParams <&> \pparams ->
          let feePerByte =
                fromIntegral (Shelley.protocolParamTxFeePerByte pparams)
              -- the required number of bytes is calculated twice to
              -- be able to handle a possible integer overflow
              feeBytes =
                bytesNeeded (fee + bytesNeeded fee * feePerByte)
           in fee + feeBytes * feePerByte
      where
        -- Calculates the number of bytes that a given integer will take
        -- when CBOR encoded.
        bytesNeeded :: Integer -> Integer
        bytesNeeded n =
          let predicate = (>= succ (Math.integerLog2 n `div` 8))
           in fromIntegral . fromJust $ -- using `fromJust` here is safe
                List.find predicate [2 ^ x | x <- [(0 :: Int) ..]]

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $
    Ledger.Scripts.applyArguments script args

{- | Computes the execution units needed for each script in the transaction.
 https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.Fees.html#evaluateTransactionExecutionUnits
-}
evalTxExecutionUnits :: EvalExUnitsRequest -> AppM ExecutionUnitsMap
evalTxExecutionUnits EvalExUnitsRequest {tx} =
  case decodeCborTx tx of
    Left err ->
      throwM (CborDecode err)
    Right (C.Tx txBody@(C.TxBody txBodyContent) _) -> do
      -- performing the necessary node queries to obtain the parameters
      -- required by the Cardano API `evaluateTransactionExecutionUnits`
      sysStart <- queryNode C.QuerySystemStart
      eraHistory <- queryNode (C.QueryEraHistory C.CardanoModeIsMultiEra)
      pparams <- asks protocolParams
      let eraInMode = C.AlonzoEraInCardanoMode
          txInputs = Set.fromList . fmap fst $ Shelley.txIns txBodyContent
          eval = C.evaluateTransactionExecutionUnits
      -- utxos must cover all the inputs referenced by the transaction,
      -- otherwise `ScriptErrorMissingTxIn` will be thrown
      utxos <- queryUtxos txInputs
      case eval eraInMode sysStart eraHistory pparams utxos txBody of
        Left err ->
          throwM (CardanoError . TxValidityIntervalError $ C.displayError err)
        Right mp ->
          asExecutionUnitsMap mp
  where
    asExecutionUnitsMap ::
      Map.Map
        C.ScriptWitnessIndex
        (Either C.ScriptExecutionError C.ExecutionUnits) ->
      AppM ExecutionUnitsMap
    asExecutionUnitsMap mp = do
      exUnitsMap <-
        for mp $
          either (throwM . CardanoError . ScriptExecutionError) pure
      pure . ExecutionUnitsMap $
        (first Shelley.toAlonzoRdmrPtr <$> Map.toList exUnitsMap)
          <&> \(TxWitness.RdmrPtr tag idx, exUnits) ->
            RdmrPtrExUnits
              { rdmrPtrTag = (toEnum . fromEnum) tag
              , rdmrPtrIdx = idx
              , exUnitsMem = C.executionMemory exUnits
              , exUnitsSteps = C.executionSteps exUnits
              }

--------------------------------------------------------------------------------
-- Estimate fee
--------------------------------------------------------------------------------

{- | Calculates the transaction fee for the proposed Alonzo era transaction,
 including the script fees.
 https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.Fees.html#evaluateTransactionFee
-}
estimateFee ::
  Shelley.ProtocolParameters -> Word -> C.Tx C.AlonzoEra -> Integer
estimateFee pparams numWits (C.Tx txBody _) =
  let estimate :: Integer
      C.Lovelace estimate =
        C.evaluateTransactionFee
          pparams
          txBody
          numWits
          -- No. of Byron key witnesses; there shouldn't be any of these and
          -- 'evaluateTransactionFee' won't work with them anyway
          0
   in estimate

--------------------------------------------------------------------------------
-- Query node
--------------------------------------------------------------------------------

queryNode :: forall (r :: Type). C.QueryInMode C.CardanoMode r -> AppM r
queryNode query = do
  nodeConnectInfo <- getNodeConnectInfo
  response <- liftIO $ C.queryNodeLocalState nodeConnectInfo Nothing query
  either (throwM . CardanoError . AcquireFailure . show) pure response

queryUtxos :: Set.Set C.TxIn -> AppM (C.UTxO C.AlonzoEra)
queryUtxos txInputs =
  either (\_ -> throwM $ CardanoError EraMismatchError) pure
    =<< ( queryNode
            . C.QueryInEra C.AlonzoEraInCardanoMode
            . C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo
            $ C.QueryUTxO (C.QueryUTxOByTxIn txInputs)
        )

--------------------------------------------------------------------------------
-- Set script integrity hash
--------------------------------------------------------------------------------

setScriptIntegrityHash ::
  Shelley.ProtocolParameters ->
  Shelley.TxBody C.AlonzoEra ->
  Shelley.TxBody C.AlonzoEra
setScriptIntegrityHash pparams txBodyAlonzo =
  case txBodyAlonzo of
    Shelley.ShelleyTxBody e txBody scripts scriptData a v ->
      case scriptData of
        Shelley.TxBodyScriptData _ datums redeemers ->
          let noScripts = null scripts
              mbIntegrityHash =
                hashScriptIntegrity pparams txBody redeemers datums noScripts
              newTxBody =
                txBody {Tx.scriptIntegrityHash = mbIntegrityHash}
           in Shelley.ShelleyTxBody e newTxBody scripts scriptData a v
        _ -> txBodyAlonzo

hashScriptIntegrity ::
  Shelley.ProtocolParameters ->
  Ledger.TxBody (Alonzo.AlonzoEra StandardCrypto) ->
  TxWitness.Redeemers (Alonzo.AlonzoEra StandardCrypto) ->
  TxWitness.TxDats (Alonzo.AlonzoEra StandardCrypto) ->
  Bool ->
  StrictMaybe (Tx.ScriptIntegrityHash StandardCrypto)
hashScriptIntegrity pparams' txBody redeemers datums noScripts =
  let pparams =
        C.toLedgerPParams C.ShelleyBasedEraAlonzo pparams'
      Value.Value ada assets =
        Tx.mint txBody
      languages
        | noScripts && Map.null assets && ada == 0 = mempty
        | TxWitness.nullRedeemers redeemers = mempty
        | otherwise = Set.fromList [PlutusV1]
   in Tx.hashScriptIntegrity pparams languages redeemers datums

--------------------------------------------------------------------------------
-- Encoding / Decoding
--------------------------------------------------------------------------------

decodeCborText :: Cbor -> Either CborDecodeError ByteString
decodeCborText (Cbor cborText) =
  first InvalidHex . Base16.decode $
    Text.Encoding.encodeUtf8 cborText

decodeCborTx :: Cbor -> Either CborDecodeError (C.Tx C.AlonzoEra)
decodeCborTx cbor =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decodeCborText cbor
