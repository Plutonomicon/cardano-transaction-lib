{-# LANGUAGE NamedFieldPuns #-}

module Api.Handlers (
  estimateTxFees,
  applyArgs,
  hashScript,
  finalizeTx
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (Proxy))
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

import Cardano.Binary qualified as Cbor
import Control.Monad.IO.Class

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
finalizeTx req@(FinalizeRequest {tx}) = do
  liftIO $ print $ "cbor:"
  liftIO $ print $ show req
  decoded <- either (throwM . FinalizeTx . decodeErrorToFinalizeError) pure $
    decodeCborTx tx
  liftIO $ print decoded
  pure (FinalizedTransaction undefined)
  where
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

decodeCborTx :: Cbor -> Either DecodeError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decodeCborText txt

decodeCborText :: Text -> Either DecodeError ByteString
decodeCborText = first InvalidHex . Base16.decode . Text.Encoding.encodeUtf8

decodeCborDatum :: Cbor -> Either DecodeError C.ScriptData
decodeCborDatum (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType (Proxy :: Proxy C.ScriptData))
    =<< decodeCborText txt
