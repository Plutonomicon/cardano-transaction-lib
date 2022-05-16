{-# LANGUAGE NamedFieldPuns #-}

module Api.Handlers (
  estimateTxFees,
  applyArgs,
  hashData,
  hashScript,
  blake2bHash,
  evalTxExecutionUnits,
  finalizeTx,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary (Annotator (runAnnotator), FullByteString (Full))
import Cardano.Binary qualified as Cbor
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.Data qualified as Data
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Tx qualified as Tx
import Cardano.Ledger.Alonzo.TxWitness qualified as TxWitness
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value qualified as Value
import Cardano.Ledger.SafeHash qualified as SafeHash
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text.Encoding
import Data.Traversable (for)
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import PlutusTx.Builtins qualified as PlutusTx
import Types (
  AppM,
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  Blake2bHash (Blake2bHash),
  BytesToHash (BytesToHash),
  CardanoError (
    AcquireFailure,
    EraMismatchError,
    ScriptExecutionError,
    TxValidityIntervalError
  ),
  Cbor (Cbor),
  CborDecodeError (InvalidCbor, InvalidHex, OtherDecodeError),
  CtlServerError (CardanoError, CborDecode),
  Env (protocolParams),
  ExecutionUnitsMap (..),
  Fee (Fee),
  FinalizeRequest (..),
  FinalizedTransaction (..),
  HashDataRequest (HashDataRequest),
  HashScriptRequest (HashScriptRequest),
  HashedData (HashedData),
  HashedScript (HashedScript),
  RdmrPtrExUnits (..),
  WitnessCount (WitnessCount),
  getNodeConnectInfo,
  hashLedgerScript,
 )

estimateTxFees :: WitnessCount -> Cbor -> AppM Fee
estimateTxFees (WitnessCount numWits) cbor = do
  decoded <-
    either (throwM . CborDecode) pure $
      decodeCborTx cbor
  pparams <- asks protocolParams
  pure . Fee $ estimateFee pparams numWits decoded

evalTxExecutionUnits :: Cbor -> AppM ExecutionUnitsMap
evalTxExecutionUnits cbor =
  case decodeCborTx cbor of
    Left err ->
      throwM (CborDecode err)
    Right (C.Tx txBody@(C.TxBody txBodyContent) _) -> do
      sysStart <- queryNode C.QuerySystemStart
      eraHistory <- queryNode (C.QueryEraHistory C.CardanoModeIsMultiEra)
      pparams <- asks protocolParams
      let eraInMode = C.AlonzoEraInCardanoMode
          txInputs = Set.fromList . fmap fst $ Shelley.txIns txBodyContent
          eval = C.evaluateTransactionExecutionUnits
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

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $
    Ledger.Scripts.applyArguments script args

hashScript :: HashScriptRequest -> AppM HashedScript
hashScript (HashScriptRequest script) =
  pure . HashedScript $ hashLedgerScript script

hashData :: HashDataRequest -> AppM HashedData
hashData (HashDataRequest datum) = do
  decodedDatum <-
    throwDecodeErrorWithMessage "Failed to decode Datum" $
      decodeCborDatum datum
  pure . HashedData . SafeHash.originalBytes $
    Data.hashData decodedDatum

throwDecodeErrorWithMessage :: forall (a :: Type). String -> Maybe a -> AppM a
throwDecodeErrorWithMessage msg =
  maybe (throwM . CborDecode $ OtherDecodeError msg) pure

blake2bHash :: BytesToHash -> AppM Blake2bHash
blake2bHash (BytesToHash hs) =
  pure . Blake2bHash . PlutusTx.fromBuiltin . PlutusTx.blake2b_256 $
    PlutusTx.toBuiltin hs

finalizeTx :: FinalizeRequest -> AppM FinalizedTransaction
finalizeTx (FinalizeRequest {tx, datums, redeemers}) = do
  pparams <- asks protocolParams
  decodedTx <-
    throwDecodeErrorWithMessage "Failed to decode tx" $
      decodeCborValidatedTx tx
  decodedRedeemers <-
    throwDecodeErrorWithMessage "Failed to decode redeemers" $
      decodeCborRedeemers redeemers
  decodedDatums <-
    throwDecodeErrorWithMessage "Failed to decode datums" $
      traverse decodeCborDatum datums
  let scripts = Tx.txscripts' $ Tx.wits decodedTx
      Value.Value ada assets = Tx.mint $ Tx.body decodedTx
      languages
        | Map.null scripts && Map.null assets && ada == 0 = mempty
        | TxWitness.nullRedeemers decodedRedeemers = mempty
        | otherwise = Set.fromList [PlutusV1]
      txDatums =
        TxWitness.TxDats . Map.fromList $
          decodedDatums <&> \datum -> (Data.hashData datum, datum)
      mbIntegrityHash =
        Tx.hashScriptIntegrity
          (C.toLedgerPParams C.ShelleyBasedEraAlonzo pparams)
          languages
          decodedRedeemers
          txDatums
  let addIntegrityHash t =
        t
          { Tx.body =
              Tx.body t
                & \body -> body {Tx.scriptIntegrityHash = mbIntegrityHash}
          }
      addDatumsAndRedeemers t =
        t
          { Tx.wits =
              Tx.wits t
                & \witness ->
                  witness
                    { TxWitness.txdats = txDatums
                    , TxWitness.txrdmrs = decodedRedeemers
                    }
          }
      finalizedTx = addIntegrityHash $ addDatumsAndRedeemers decodedTx
      response =
        FinalizedTransaction . encodeCborText . Cbor.serializeEncoding $
          Tx.toCBORForMempoolSubmission finalizedTx
  pure response

-- Helpers

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

queryNode :: forall (r :: Type). C.QueryInMode C.CardanoMode r -> AppM r
queryNode query = do
  nodeConnectInfo <- getNodeConnectInfo
  response <- liftIO $ C.queryNodeLocalState nodeConnectInfo Nothing query
  either (throwM . CardanoError . AcquireFailure . show) pure $
    response

queryUtxos :: Set.Set C.TxIn -> AppM (C.UTxO C.AlonzoEra)
queryUtxos txInputs =
  either (\_ -> throwM $ CardanoError EraMismatchError) pure
    =<< ( queryNode
            . C.QueryInEra C.AlonzoEraInCardanoMode
            . C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo
            $ C.QueryUTxO (C.QueryUTxOByTxIn txInputs)
        )

decodeCborText :: Cbor -> Either CborDecodeError ByteString
decodeCborText (Cbor cborText) =
  first InvalidHex . Base16.decode $
    Text.Encoding.encodeUtf8 cborText

encodeCborText :: BL.ByteString -> Cbor
encodeCborText = Cbor . Text.Encoding.decodeUtf8 . Base16.encode . BL.toStrict

decodeCborTx :: Cbor -> Either CborDecodeError (C.Tx C.AlonzoEra)
decodeCborTx cbor =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decodeCborText cbor

decodeCborValidatedTx ::
  Cbor -> Maybe (Tx.ValidatedTx (Alonzo.AlonzoEra StandardCrypto))
decodeCborValidatedTx = decodeCborComponent

decodeCborRedeemers ::
  Cbor -> Maybe (TxWitness.Redeemers (Alonzo.AlonzoEra StandardCrypto))
decodeCborRedeemers = decodeCborComponent

decodeCborDatum ::
  Cbor -> Maybe (Data.Data (Alonzo.AlonzoEra StandardCrypto))
decodeCborDatum = decodeCborComponent

decodeCborComponent ::
  forall (a :: Type). Cbor.FromCBOR (Cbor.Annotator a) => Cbor -> Maybe a
decodeCborComponent cbor = do
  bs <- preview _Right $ decodeCborTextLazyBS cbor
  fmap (`runAnnotator` Full bs) . preview _Right . fmap snd $
    deserialiseFromBytes Cbor.fromCBOR bs

decodeCborTextLazyBS :: Cbor -> Either CborDecodeError BL.ByteString
decodeCborTextLazyBS (Cbor text) =
  bimap InvalidHex BL.fromStrict
    . Base16.decode
    $ Text.Encoding.encodeUtf8 text
