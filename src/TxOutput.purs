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
  , utxoIndexToUtxo
  ) where

import Prelude

import Address
  ( addressToOgmiosAddress
  , enterpriseAddressValidatorHash
  , ogmiosAddressToAddress
  )
import Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , Utxo
  ) as Transaction
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)
import Scripts (validatorHashEnterpriseAddress)
import Serialization.Address (NetworkId)
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.Datum (DataHash)
import QueryM.Ogmios as Ogmios
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
ogmiosTxOutToTransactionOutput { address, value, datum } = do
  address' <- ogmiosAddressToAddress address
  -- If datum ~ Maybe String is Nothing, do nothing. Otherwise, attempt to hash
  -- and capture failure if we can't hash.
  dataHash <-
    maybe (Just Nothing) (map Just <<< ogmiosDatumHashToDatumHash) datum
  pure $ wrap
    { address: address'
    , amount: value
    , dataHash
    }

-- | Converts an internal transaction output to the Ogmios transaction output.
transactionOutputToOgmiosTxOut
  :: Transaction.TransactionOutput -> Ogmios.OgmiosTxOut
transactionOutputToOgmiosTxOut
  (Transaction.TransactionOutput { address, amount: value, dataHash }) =
  { address: addressToOgmiosAddress address
  , value
  , datum: datumHashToOgmiosDatumHash <$> dataHash
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
  }

-- | Converts an internal transaction output to `ScriptOutput`.
transactionOutputToScriptOutput
  :: Transaction.TransactionOutput -> Maybe UTx.ScriptOutput
transactionOutputToScriptOutput
  ( Transaction.TransactionOutput
      { address, amount: value, dataHash: Just datumHash }
  ) = do
  validatorHash <- enterpriseAddressValidatorHash address
  pure $ UTx.ScriptOutput
    { validatorHash
    , value
    , datumHash
    }
transactionOutputToScriptOutput
  (Transaction.TransactionOutput { dataHash: Nothing }) = Nothing

-- | Converts `ScriptOutput` to an internal transaction output.
scriptOutputToTransactionOutput
  :: NetworkId -> UTx.ScriptOutput -> Transaction.TransactionOutput
scriptOutputToTransactionOutput
  networkId
  (UTx.ScriptOutput { validatorHash, value, datumHash }) =
  Transaction.TransactionOutput
    { address: validatorHashEnterpriseAddress networkId validatorHash
    , amount: value
    , dataHash: Just datumHash
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

--------------------------------------------------------------------------------
-- Conversion between Utxo types
--------------------------------------------------------------------------------
-- | Converts a utxoIndex from `UnbalancedTx` to `Utxo`.
utxoIndexToUtxo
  :: NetworkId
  -> Map Transaction.TransactionInput UTx.ScriptOutput
  -> Transaction.Utxo
utxoIndexToUtxo networkId = map (scriptOutputToTransactionOutput networkId)
