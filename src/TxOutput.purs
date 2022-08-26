module TxOutput
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

import Address
  ( addressToOgmiosAddress
  , enterpriseAddressValidatorHash
  , ogmiosAddressToAddress
  )
import Cardano.Types.Transaction (TransactionOutput(TransactionOutput)) as Transaction
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import QueryM.Ogmios as Ogmios
import Scripts (validatorHashEnterpriseAddress)
import Serialization.Address (NetworkId)
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.Datum (DataHash)
import Types.OutputDatum
  ( OutputDatum(OutputDatumHash, NoOutputDatum)
  , outputDatumDataHash
  )
import Types.Transaction (TransactionInput(TransactionInput)) as Transaction
import Types.UnbalancedTransaction as UTx

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

-- https://ogmios.dev/ogmios.wsp.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now. https://github.com/Plutonomicon/cardano-transaction-lib/issues/78
-- | Converts an Ogmios transaction output to (internal) `TransactionOutput`
ogmiosTxOutToTransactionOutput
  :: Ogmios.OgmiosTxOut -> Maybe Transaction.TransactionOutput
ogmiosTxOutToTransactionOutput { address, value, datum, script } = do
  address' <- ogmiosAddressToAddress address
  -- If datum ~ Maybe String is Nothing, do nothing. Otherwise, attempt to hash
  -- and capture failure if we can't hash.
  dataHash <- traverse ogmiosDatumHashToDatumHash datum
  pure $ wrap
    { address: address'
    , amount: value
    -- TODO: populate properly
    -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/691
    , datum: maybe NoOutputDatum OutputDatumHash dataHash
    , scriptRef: script
    }

-- | Converts an internal transaction output to the Ogmios transaction output.
transactionOutputToOgmiosTxOut
  :: Transaction.TransactionOutput -> Ogmios.OgmiosTxOut
transactionOutputToOgmiosTxOut
  (Transaction.TransactionOutput { address, amount: value, datum, scriptRef }) =
  { address: addressToOgmiosAddress address
  , value
  , datum: datumHashToOgmiosDatumHash <$> outputDatumDataHash datum
  , script: scriptRef
  }

-- | Converts an Ogmios Transaction output to a `ScriptOutput`.
ogmiosTxOutToScriptOutput :: Ogmios.OgmiosTxOut -> Maybe UTx.ScriptOutput
ogmiosTxOutToScriptOutput { address, value, datum: Just dHash } = do
  address' <- ogmiosAddressToAddress address
  validatorHash <- enterpriseAddressValidatorHash address'
  datumHash <- ogmiosDatumHashToDatumHash dHash
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datumHash
    }
ogmiosTxOutToScriptOutput { datum: Nothing } = Nothing

-- | Converts an `ScriptOutput` to Ogmios Transaction output.
scriptOutputToOgmiosTxOut
  :: NetworkId -> UTx.ScriptOutput -> Ogmios.OgmiosTxOut
scriptOutputToOgmiosTxOut
  networkId
  (UTx.ScriptOutput { validatorHash, value, datumHash }) =
  { address:
      addressToOgmiosAddress $ validatorHashEnterpriseAddress networkId
        validatorHash
  , value
  , datum: pure (datumHashToOgmiosDatumHash datumHash)
  , script: Nothing -- TODO: Update or deprecate `ScriptOutput` 
  }

-- | Converts an internal transaction output to `ScriptOutput`.
transactionOutputToScriptOutput
  :: Transaction.TransactionOutput -> Maybe UTx.ScriptOutput
transactionOutputToScriptOutput
  ( Transaction.TransactionOutput
      { address, amount: value, datum: OutputDatumHash datumHash }
  ) = do
  validatorHash <- enterpriseAddressValidatorHash address
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datumHash
    }
transactionOutputToScriptOutput
  (Transaction.TransactionOutput _) = Nothing

-- | Converts `ScriptOutput` to an internal transaction output.
scriptOutputToTransactionOutput
  :: NetworkId -> UTx.ScriptOutput -> Transaction.TransactionOutput
scriptOutputToTransactionOutput
  networkId
  (UTx.ScriptOutput { validatorHash, value, datumHash }) =
  Transaction.TransactionOutput
    { address: validatorHashEnterpriseAddress networkId validatorHash
    , amount: value
    , datum: OutputDatumHash datumHash
    , scriptRef: Nothing
    }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------
-- | Converts an Ogmios datum hash `String` to an internal `DataHash`
ogmiosDatumHashToDatumHash :: String -> Maybe DataHash
ogmiosDatumHashToDatumHash str = hexToByteArray str <#> wrap

-- | Converts an internal `DataHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DataHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap
