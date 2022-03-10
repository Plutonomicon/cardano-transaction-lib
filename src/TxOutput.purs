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
  , addressValidatorHash
  , ogmiosAddressToAddress
  )
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)
import Scripts (validatorHashAddress)
import Serialization.Address (NetworkId)
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.Datum (DatumHash)
import Types.JsonWsp as JsonWsp
import Types.Transaction as Transaction
import Types.UnbalancedTransaction as UTx

-- | A module for helpers of the various transaction output types.

--------------------------------------------------------------------------------
-- Conversion between transaction input/output types
--------------------------------------------------------------------------------
-- I think txId is a hexadecimal encoding.
-- | Converts an Ogmios `TxOutRef` to (internal) `TransactionInput`
txOutRefToTransactionInput
  :: JsonWsp.TxOutRef -> Maybe Transaction.TransactionInput
txOutRefToTransactionInput { txId, index } = do
  transaction_id <- hexToByteArray txId <#> wrap
  pure $ wrap
    { transaction_id
    , index
    }

-- | Converts an (internal) `TransactionInput` to an Ogmios `TxOutRef`
transactionInputToTxOutRef
  :: Transaction.TransactionInput -> JsonWsp.TxOutRef
transactionInputToTxOutRef
  (Transaction.TransactionInput { transaction_id, index }) =
  { txId: byteArrayToHex (unwrap transaction_id)
  , index
  }

-- https://ogmios.dev/ogmios.wsp.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now. https://github.com/Plutonomicon/cardano-browser-tx/issues/78
-- | Converts an Ogmios `TxOut` to (internal) `TransactionOutput`
ogmiosTxOutToTransactionOutput
  :: JsonWsp.OgmiosTxOut -> Maybe Transaction.TransactionOutput
ogmiosTxOutToTransactionOutput { address, value, datum } = do
  address' <- ogmiosAddressToAddress address
  -- If datum ~ Maybe String is Nothing, do nothing. Otherwise, attempt to hash
  -- and capture failure if we can't hash.
  data_hash <-
    maybe (Just Nothing) (map Just <<< ogmiosDatumHashToDatumHash) datum
  pure $ wrap
    { address: address'
    , amount: value
    , data_hash
    }

-- | Converts an internal transaction output to the Ogmios transaction output.
transactionOutputToOgmiosTxOut
  :: Transaction.TransactionOutput -> JsonWsp.OgmiosTxOut
transactionOutputToOgmiosTxOut
  (Transaction.TransactionOutput { address, amount: value, data_hash }) =
  { address: addressToOgmiosAddress address
  , value
  , datum: datumHashToOgmiosDatumHash <$> data_hash
  }

-- | Converts an Ogmios Transaction output to a `ScriptOutput`.
ogmiosTxOutToScriptOutput :: JsonWsp.OgmiosTxOut -> Maybe UTx.ScriptOutput
ogmiosTxOutToScriptOutput { address, value, datum: Just dHash } = do
  address' <- ogmiosAddressToAddress address
  validatorHash <- addressValidatorHash address'
  datumHash <- ogmiosDatumHashToDatumHash dHash
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datumHash
    }
ogmiosTxOutToScriptOutput { datum: Nothing } = Nothing

-- | Converts an `ScriptOutput` to Ogmios Transaction output.
scriptOutputToOgmiosTxOut
  :: NetworkId -> UTx.ScriptOutput -> JsonWsp.OgmiosTxOut
scriptOutputToOgmiosTxOut
  networkId
  (UTx.ScriptOutput { validatorHash, value, datumHash }) =
  { address:
      addressToOgmiosAddress $ validatorHashAddress networkId validatorHash
  , value
  , datum: pure (datumHashToOgmiosDatumHash datumHash)
  }

-- | Converts an internal transaction output to `ScriptOutput`.
transactionOutputToScriptOutput
  :: Transaction.TransactionOutput -> Maybe UTx.ScriptOutput
transactionOutputToScriptOutput
  ( Transaction.TransactionOutput
      { address, amount: value, data_hash: Just datumHash }
  ) = do
  validatorHash <- addressValidatorHash address
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datumHash
    }
transactionOutputToScriptOutput
  (Transaction.TransactionOutput { data_hash: Nothing }) = Nothing

-- | Converts `ScriptOutput` to an internal transaction output.
scriptOutputToTransactionOutput
  :: NetworkId -> UTx.ScriptOutput -> Transaction.TransactionOutput
scriptOutputToTransactionOutput
  networkId
  (UTx.ScriptOutput { validatorHash, value, datumHash }) =
  Transaction.TransactionOutput
    { address: validatorHashAddress networkId validatorHash
    , amount: value
    , data_hash: Just datumHash
    }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------
-- | Converts an Ogmios datum hash `String` to an internal `DatumHash`
ogmiosDatumHashToDatumHash :: String -> Maybe DatumHash
ogmiosDatumHashToDatumHash str = hexToByteArray str <#> wrap

-- | Converts an internal `DatumHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DatumHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap