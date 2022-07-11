module Api.Handlers (
  estimateTxFees,
  applyArgs,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.Tx qualified as Tx
import Control.Lens ((&), (<&>))
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List (find)
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding qualified as Text.Encoding
import Math.NumberTheory.Logarithms qualified as Math (integerLog2)
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import Types (
  AppM,
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  Cbor (Cbor),
  CborDecodeError (InvalidCbor, InvalidHex),
  CtlServerError (CborDecode),
  Env (protocolParams),
  Fee (Fee),
  FeesRequest (FeesRequest, count, tx),
  WitnessCount (WitnessCount),
 )

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

estimateTxFees :: FeesRequest -> AppM Fee
estimateTxFees FeesRequest {count, tx} = do
  tx' <- decodeCborTx tx & either (throwM . CborDecode) pure
  pparams <- asks protocolParams

  let witCount :: Word
      WitnessCount witCount = count

      fee :: Integer
      fee = estimateFee pparams witCount (withScriptIntegrityHash tx')
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
           in -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/638
              fee + feeBytes * feePerByte + 2 * feePerByte
      where
        -- Calculates the number of bytes that a given integer will take
        -- when CBOR encoded.
        bytesNeeded :: Integer -> Integer
        bytesNeeded n =
          let predicate = (>= succ (Math.integerLog2 n `div` 8))
           in fromIntegral . fromJust $ -- using `fromJust` here is safe
                List.find predicate [2 ^ x | x <- [(0 :: Int) ..]]

    -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/570
    withScriptIntegrityHash :: C.Tx C.AlonzoEra -> C.Tx C.AlonzoEra
    withScriptIntegrityHash transaction@(C.Tx txBodyAlonzo keyWits) =
      case txBodyAlonzo of
        Shelley.ShelleyTxBody era txBody scr sData aux val ->
          case Tx.scriptIntegrityHash txBody of
            SNothing ->
              let txBody' = txBody {Tx.scriptIntegrityHash = SNothing}
                  shelleyB = Shelley.ShelleyTxBody era txBody' scr sData aux val
               in C.Tx shelleyB keyWits
            _ -> transaction

applyArgs :: ApplyArgsRequest -> AppM AppliedScript
applyArgs ApplyArgsRequest {script, args} =
  pure . AppliedScript $
    Ledger.Scripts.applyArguments script args

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
