module Ctl.Internal.TxOutput
  ( datumHashToOgmiosDatumHash
  , ogmiosDatumHashToDatumHash
  , ogmiosTxOutToScriptOutput
  , ogmiosTxOutToTransactionOutput
  , scriptOutputToOgmiosTxOut
  , scriptOutputToTransactionOutput
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  , transactionOutputToScriptOutput
  , txOutRefToTransactionInput
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.Address
  ( addressToOgmiosAddress
  , enterpriseAddressValidatorHash
  , ogmiosAddressToAddress
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  ) as Transaction
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData as Deserialization
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Scripts (validatorHashEnterpriseAddress)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Serialization.PlutusData as Serialization
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (hexToCborBytes)
import Ctl.Internal.Types.Datum (DataHash, Datum(Datum))
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(OutputDatum, OutputDatumHash, NoOutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  )
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput)) as Transaction
import Ctl.Internal.Types.UnbalancedTransaction as UTx
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isNothing)
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
  transactionId <- hexToByteArray txId <#> (wrap >>> wrap)
  pure $ wrap
    { transactionId
    , index
    }

-- | Converts an (internal) `TransactionInput` to an Ogmios transaction input
transactionInputToTxOutRef
  :: Transaction.TransactionInput -> Ogmios.OgmiosTxOutRef
transactionInputToTxOutRef
  (Transaction.TransactionInput { transactionId, index }) =
  { txId: byteArrayToHex (unwrap $ unwrap transactionId)
  , index
  }

-- https://ogmios.dev/ogmios.wsp.json see "datum", potential FIX ME: it says
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
  , datum: datumToOgmiosDatum =<< outputDatumDatum datum
  , script: scriptRef
  }

-- | Converts an Ogmios Transaction output to a `ScriptOutput`.
ogmiosTxOutToScriptOutput :: Ogmios.OgmiosTxOut -> Maybe UTx.ScriptOutput
ogmiosTxOutToScriptOutput { address, value, datum, datumHash } = do
  scriptDatum <- ogmiosDatumToScriptDatum datum datumHash
  address' <- ogmiosAddressToAddress address
  validatorHash <- enterpriseAddressValidatorHash address'

  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datum: scriptDatum
    }

-- | Converts an `ScriptOutput` to Ogmios Transaction output.
scriptOutputToOgmiosTxOut
  :: NetworkId -> UTx.ScriptOutput -> Ogmios.OgmiosTxOut
scriptOutputToOgmiosTxOut
  networkId
  (UTx.ScriptOutput { validatorHash, value, datum: scriptDatum }) =
  let
    address =
      addressToOgmiosAddress $ validatorHashEnterpriseAddress networkId
        validatorHash
  in
    case scriptDatum of
      UTx.ScriptDatum d ->
        { address
        , value
        , datumHash: Nothing
        , datum: datumToOgmiosDatum d
        , script: Nothing -- TODO: Update or deprecate `ScriptOutput` 
        }
      UTx.ScriptDatumHash dh ->
        { address
        , value
        , datumHash: Just (datumHashToOgmiosDatumHash dh)
        , datum: Nothing
        , script: Nothing
        }

-- | Converts an internal transaction output to `ScriptOutput`.
transactionOutputToScriptOutput
  :: Transaction.TransactionOutput -> Maybe UTx.ScriptOutput
transactionOutputToScriptOutput
  ( Transaction.TransactionOutput
      { address, amount: value, datum }
  ) = do
  scriptDatum <- outputDatumToScriptDatum datum
  validatorHash <- enterpriseAddressValidatorHash address
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datum: scriptDatum
    }

-- | Converts `ScriptOutput` to an internal transaction output.
scriptOutputToTransactionOutput
  :: NetworkId -> UTx.ScriptOutput -> Transaction.TransactionOutput
scriptOutputToTransactionOutput
  networkId
  (UTx.ScriptOutput { validatorHash, value, datum }) =
  Transaction.TransactionOutput
    { address: validatorHashEnterpriseAddress networkId validatorHash
    , amount: value
    , datum: scriptDatumToOutputDatum datum
    , scriptRef: Nothing
    }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------
-- | Converts an Ogmios datum hash `String` to an internal `DataHash`
ogmiosDatumHashToDatumHash :: String -> Maybe DataHash
ogmiosDatumHashToDatumHash str = hexToByteArray str <#> (wrap >>> wrap)

-- | Converts an Ogmios datum `String` to an internal `Datum`
ogmiosDatumToDatum :: String -> Maybe Datum
ogmiosDatumToDatum =
  hexToCborBytes
    >=> fromBytes
    >=> Deserialization.convertPlutusData
      >>> map Datum

-- | Converts an internal `DataHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DataHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap <<< unwrap

-- | Converts an internal `Datum` to an Ogmios datum `String`
datumToOgmiosDatum :: Datum -> Maybe String
datumToOgmiosDatum (Datum plutusData) =
  Serialization.convertPlutusData plutusData <#>
    (toBytes >>> unwrap >>> byteArrayToHex)

toOutputDatum :: Maybe Datum -> Maybe DataHash -> OutputDatum
toOutputDatum d dh =
  OutputDatum <$> d <|> OutputDatumHash <$> dh # fromMaybe NoOutputDatum

ogmiosDatumToScriptDatum
  :: Maybe String -> Maybe String -> Maybe UTx.ScriptDatum
ogmiosDatumToScriptDatum d dh =
  let
    datum = d >>= ogmiosDatumToDatum <#> UTx.ScriptDatum
    datumHash = dh >>= ogmiosDatumHashToDatumHash <#> UTx.ScriptDatumHash
  in
    datum <|> datumHash

outputDatumToScriptDatum :: OutputDatum -> Maybe UTx.ScriptDatum
outputDatumToScriptDatum (OutputDatum d) = Just (UTx.ScriptDatum d)
outputDatumToScriptDatum (OutputDatumHash d) = Just (UTx.ScriptDatumHash d)
outputDatumToScriptDatum NoOutputDatum = Nothing

scriptDatumToOutputDatum :: UTx.ScriptDatum -> OutputDatum
scriptDatumToOutputDatum (UTx.ScriptDatum d) = OutputDatum d
scriptDatumToOutputDatum (UTx.ScriptDatumHash dh) = OutputDatumHash dh
