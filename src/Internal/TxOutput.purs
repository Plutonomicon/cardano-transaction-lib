module Ctl.Internal.TxOutput
  ( datumHashToOgmiosDatumHash
  , ogmiosDatumHashToDatumHash
  , ogmiosTxOutToTransactionOutput
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  , txOutRefToTransactionInput
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Types
  ( DataHash
  , PlutusData
  , TransactionInput(TransactionInput)
  )
import Cardano.Types.Address as Address
import Cardano.Types.OutputDatum
  ( OutputDatum(OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  )
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Maybe (Maybe, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)

-- | A module for helpers of the various transaction output types.

--------------------------------------------------------------------------------
-- Conversion between transaction input/output types
--------------------------------------------------------------------------------
-- I think txId is a hexadecimal encoding.
-- | Converts an Ogmios transaction input to (internal) `TransactionInput`
txOutRefToTransactionInput
  :: Ogmios.OgmiosTxOutRef -> Maybe TransactionInput
txOutRefToTransactionInput { txId, index } = do
  transactionId <- hexToByteArray txId >>= fromBytes >>> map wrap
  pure $ wrap
    { transactionId
    , index
    }

-- | Converts an (internal) `TransactionInput` to an Ogmios transaction input
transactionInputToTxOutRef
  :: TransactionInput -> Ogmios.OgmiosTxOutRef
transactionInputToTxOutRef
  (TransactionInput { transactionId, index }) =
  { txId: byteArrayToHex (toBytes $ unwrap transactionId)
  , index
  }

-- https://ogmios.dev/ogmios.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now. https://github.com/Plutonomicon/cardano-transaction-lib/issues/78
-- | Converts an Ogmios transaction output to (internal) `TransactionOutput`
ogmiosTxOutToTransactionOutput
  :: Ogmios.OgmiosTxOut -> Maybe TransactionOutput
ogmiosTxOutToTransactionOutput { address, value, datum, datumHash, script } = do
  address' <- Address.fromBech32 address
  -- If datum ~ Maybe String is Nothing, do nothing. Otherwise, attempt to
  -- convert and capture failure if we can't.
  dh <- traverse ogmiosDatumHashToDatumHash datumHash
  -- For compatibility with Alonzo, don't attempt to parse the datum if
  -- datumHash is set
  d <- traverse ogmiosDatumToDatum (guard (isNothing datumHash) *> datum)
  pure $ wrap
    { address: address'
    , amount: value
    , datum: toOutputDatum d dh
    , scriptRef: script
    }

-- | Converts an internal transaction output to the Ogmios transaction output.
transactionOutputToOgmiosTxOut
  :: TransactionOutput -> Ogmios.OgmiosTxOut
transactionOutputToOgmiosTxOut
  (TransactionOutput { address, amount: value, datum, scriptRef }) =
  { address: Address.toBech32 address
  , value
  , datumHash: datumHashToOgmiosDatumHash <$> (outputDatumDataHash =<< datum)
  , datum: datumToOgmiosDatum <$> (outputDatumDatum =<< datum)
  , script: scriptRef
  }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------
-- | Converts an Ogmios datum hash `String` to an internal `DataHash`
ogmiosDatumHashToDatumHash :: String -> Maybe DataHash
ogmiosDatumHashToDatumHash str = hexToByteArray str >>= wrap >>> decodeCbor

-- | Converts an Ogmios datum `String` to an internal `Datum`
ogmiosDatumToDatum :: String -> Maybe PlutusData
ogmiosDatumToDatum =
  hexToByteArray >=> wrap >>> decodeCbor

-- | Converts an internal `DataHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DataHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap <<< encodeCbor

-- | Converts an internal `Datum` to an Ogmios datum `String`
datumToOgmiosDatum :: PlutusData -> String
datumToOgmiosDatum =
  encodeCbor >>> unwrap >>> byteArrayToHex

toOutputDatum :: Maybe PlutusData -> Maybe DataHash -> Maybe OutputDatum
toOutputDatum d dh =
  OutputDatum <$> d <|> OutputDatumHash <$> dh
