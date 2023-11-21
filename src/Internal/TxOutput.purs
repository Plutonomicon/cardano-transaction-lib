module Ctl.Internal.TxOutput
  ( datumHashToOgmiosDatumHash
  , ogmiosDatumHashToDatumHash
  , ogmiosTxOutToTransactionOutput
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  , txOutRefToTransactionInput
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  ) as Transaction
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData as Deserialization
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.PlutusData as Serialization
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (hexToCborBytes)
import Ctl.Internal.Types.Datum (DataHash, Datum(Datum))
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(OutputDatum, OutputDatumHash, NoOutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  )
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput)) as Transaction
import Data.Maybe (Maybe(Just), fromMaybe, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)

-- | A module for helpers of the various transaction output types.

--------------------------------------------------------------------------------
-- Conversion between transaction input/output types
--------------------------------------------------------------------------------
-- I think txId is a hexadecimal encoding.
-- | Converts an Ogmios transaction input to (internal) `TransactionInput`
txOutRefToTransactionInput
  :: Ogmios.OgmiosTxOutRef -> Maybe Transaction.TransactionInput
txOutRefToTransactionInput { txId, index } = do
  transactionId <- hexToByteArray txId <#> wrap
  pure $ wrap
    { transactionId
    , index
    }

-- | Converts an (internal) `TransactionInput` to an Ogmios transaction input
transactionInputToTxOutRef
  :: Transaction.TransactionInput -> Ogmios.OgmiosTxOutRef
transactionInputToTxOutRef
  (Transaction.TransactionInput { transactionId, index }) =
  { txId: byteArrayToHex (unwrap transactionId)
  , index
  }

-- https://ogmios.dev/ogmios.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now. https://github.com/Plutonomicon/cardano-transaction-lib/issues/78
-- | Converts an Ogmios transaction output to (internal) `TransactionOutput`
ogmiosTxOutToTransactionOutput
  :: Ogmios.OgmiosTxOut -> Maybe Transaction.TransactionOutput
ogmiosTxOutToTransactionOutput { address, value, datum, datumHash, script } = do
  address' <- ogmiosAddressToAddress address
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
  :: Transaction.TransactionOutput -> Ogmios.OgmiosTxOut
transactionOutputToOgmiosTxOut
  (Transaction.TransactionOutput { address, amount: value, datum, scriptRef }) =
  { address: addressToOgmiosAddress address
  , value
  , datumHash: datumHashToOgmiosDatumHash <$> outputDatumDataHash datum
  , datum: datumToOgmiosDatum <$> outputDatumDatum datum
  , script: scriptRef
  }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------
-- | Converts an Ogmios datum hash `String` to an internal `DataHash`
ogmiosDatumHashToDatumHash :: String -> Maybe DataHash
ogmiosDatumHashToDatumHash str = hexToByteArray str <#> wrap

-- | Converts an Ogmios datum `String` to an internal `Datum`
ogmiosDatumToDatum :: String -> Maybe Datum
ogmiosDatumToDatum =
  hexToCborBytes
    >=> fromBytes
    >=> (Deserialization.convertPlutusData >>> Datum >>> Just)

-- | Converts an internal `DataHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DataHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap

-- | Converts an internal `Datum` to an Ogmios datum `String`
datumToOgmiosDatum :: Datum -> String
datumToOgmiosDatum (Datum plutusData) =
  Serialization.convertPlutusData plutusData #
    toBytes >>> unwrap >>> byteArrayToHex

toOutputDatum :: Maybe Datum -> Maybe DataHash -> OutputDatum
toOutputDatum d dh =
  OutputDatum <$> d <|> OutputDatumHash <$> dh # fromMaybe NoOutputDatum
