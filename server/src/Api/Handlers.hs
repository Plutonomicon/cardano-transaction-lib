{-# LANGUAGE NamedFieldPuns #-}

module Api.Handlers (
  estimateTxFees,
  applyArgs,
  hashScript,
  finalizeTx
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary (Annotator(runAnnotator), FullByteString(Full))
import Cardano.Binary qualified as Cbor
import Cardano.Ledger.Alonzo as Alonzo
import Cardano.Ledger.Alonzo.Data as Data
import Cardano.Ledger.Alonzo.Language (Language(PlutusV1, PlutusV2))
import Cardano.Ledger.Alonzo.Tx as Tx
import Cardano.Ledger.Alonzo.TxWitness as TxWitness
import Cardano.Ledger.Crypto (StandardCrypto)
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import Types (
  AppM,
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  CardanoBrowserServerError (FeeEstimate, FinalizeTx),
  Cbor (Cbor),
  Env (protocolParams),
  Fee (Fee),
  FeeEstimateError (FEInvalidCbor, FEInvalidHex),
  FinalizeTxError (FTInvalidCbor, FTInvalidHex),
  HashScriptRequest (HashScriptRequest),
  HashedScript (HashedScript),
  FinalizeRequest (..),
  FinalizedTransaction(..),
  hashLedgerScript,
 )


estimateTxFees :: Cbor -> AppM Fee
estimateTxFees cbor = do
  decoded <- either (throwM . FeeEstimate . decodeErrorToEstimateError) pure $
    decodeCborTx cbor
  pparams <- asks protocolParams
  pure . Fee $ estimateFee pparams decoded
  where
    decodeErrorToEstimateError (InvalidCbor cbe) = FEInvalidCbor cbe
    decodeErrorToEstimateError (InvalidHex he) = FEInvalidHex he

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $
    Ledger.Scripts.applyArguments script args

hashScript :: HashScriptRequest -> AppM HashedScript
hashScript (HashScriptRequest script) =
  pure . HashedScript $ hashLedgerScript script

finalizeTx :: FinalizeRequest -> AppM FinalizedTransaction
finalizeTx req@(FinalizeRequest {tx, datums, redeemers}) = do
  liftIO $ do
    putStrLn $ "cbor:"
    putStrLn $ show req
  decodedTx <- maybe (handleError $ InvalidHex "Failed to decode Tx") pure $
    decodeCborValidatedTx tx
  decodedRedeemers <- maybe (handleError $ InvalidHex "Failed to decode Redeemers") pure $
    decodeCborRedeemers redeemers
  decodedDatums <- maybe (handleError $ InvalidHex "Failed to decode Datums") pure $
    traverse decodeCborDatum datums
  liftIO $ do
    putStrLn "tx:"
    print decodedTx
    putStrLn "redeemres:"
    print decodedRedeemers
    putStrLn "datums:"
    print decodedDatums
    let
      pparams = undefined -- TODO
      languages = Set.fromList [PlutusV1, PlutusV2]
      _mbIntegrityHash = Tx.hashScriptIntegrity pparams languages decodedRedeemers
        . TxWitness.TxDats . Map.fromList $ decodedDatums <&> \datum ->
        (Data.hashData datum, datum)
    pure (FinalizedTransaction undefined)
  where
    handleError = throwM . FinalizeTx . decodeErrorToFinalizeError
    decodeErrorToFinalizeError (InvalidCbor cbe) = FTInvalidCbor cbe
    decodeErrorToFinalizeError (InvalidHex he) = FTInvalidHex he


-- Helpers

estimateFee :: Shelley.ProtocolParameters -> C.Tx C.AlonzoEra -> Integer
estimateFee pparams (C.Tx txBody keyWits) = estimate
  where
    estimate :: Integer
    C.Lovelace estimate =
      let -- No. of Shelley key witnesses
          numWits = fromIntegral $ length keyWits
       in C.evaluateTransactionFee
            pparams
            txBody
            numWits
            -- No. of Byron key witnesses; there shouldn't be any of these and
            -- 'evaluateTransactionFee' won't work with them anyway
            0

data DecodeError = InvalidCbor Cbor.DecoderError | InvalidHex String

decodeCborText :: Text -> Either DecodeError ByteString
decodeCborText = first InvalidHex . Base16.decode . Text.Encoding.encodeUtf8

decodeCborTx :: Cbor -> Either DecodeError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decodeCborText txt

decodeCborValidatedTx ::
    Cbor -> Maybe (Tx.ValidatedTx (Alonzo.AlonzoEra StandardCrypto))
decodeCborValidatedTx cbor = do
  bs <- preview _Right $ decodeCborTextLazyBS cbor
  fmap (flip runAnnotator (Full bs)) $ preview _Right $ fmap snd $
    deserialiseFromBytes Cbor.fromCBOR bs

decodeCborTextLazyBS :: Cbor -> Either DecodeError BL.ByteString
decodeCborTextLazyBS (Cbor text) =
  first InvalidHex .
  second BL.fromStrict .
  Base16.decode $ Text.Encoding.encodeUtf8 text

decodeCborRedeemers ::
  Cbor -> Maybe (TxWitness.Redeemers (Alonzo.AlonzoEra StandardCrypto))
decodeCborRedeemers cbor = do
  bs <- preview _Right $ decodeCborTextLazyBS cbor
  fmap (flip runAnnotator (Full bs)) $  preview _Right $ fmap snd $
    deserialiseFromBytes Cbor.fromCBOR bs

decodeCborDatum ::
  Cbor -> Maybe (Data.Data (Alonzo.AlonzoEra StandardCrypto))
decodeCborDatum cbor = do
  bs <- preview _Right $ decodeCborTextLazyBS cbor
  fmap (flip runAnnotator (Full bs)) $ preview _Right $ fmap snd $
    deserialiseFromBytes Cbor.fromCBOR bs
