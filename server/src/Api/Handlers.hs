{-# LANGUAGE NamedFieldPuns #-}

module Api.Handlers (
  estimateTxFees,
  applyArgs,
  hashScript,
  blake2bHash,
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
import PlutusTx.Builtins qualified as PlutusTx
import Types (
  AppM,
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  Blake2bHash (Blake2bHash),
  CardanoBrowserServerError (FeeEstimate),
  Cbor (Cbor),
  Env (protocolParams),
  Fee (Fee),
  FeeEstimateError (InvalidCbor, InvalidHex),
  HashScriptRequest (HashScriptRequest),
  HashedScript (HashedScript),
  HexString (HexString),
  hashLedgerScript,
 )

estimateTxFees :: Cbor -> AppM Fee
estimateTxFees cbor = do
  decoded <- either (throwM . FeeEstimate) pure $ decodeCborTx cbor
  pparams <- asks protocolParams
  pure . Fee $ estimateFee pparams decoded

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $
    Ledger.Scripts.applyArguments script args

hashScript :: HashScriptRequest -> AppM HashedScript
hashScript (HashScriptRequest script) =
  pure . HashedScript $ hashLedgerScript script

blake2bHash :: HexString -> AppM Blake2bHash
blake2bHash (HexString hs) =
  pure . Blake2bHash . PlutusTx.fromBuiltin . PlutusTx.blake2b_256 $
    PlutusTx.toBuiltin hs

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

decodeCborTx :: Cbor -> Either FeeEstimateError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decode txt
  where
    decode :: Text -> Either FeeEstimateError ByteString
    decode = first InvalidHex . Base16.decode . Text.Encoding.encodeUtf8
