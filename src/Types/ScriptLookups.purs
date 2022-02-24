module Types.ScriptLookups where

import Prelude
import Data.Either (Either(Left, Right))
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Serialization.Address (Address)
import Types.PlutusData (Datum, DatumHash)
import Types.Scripts
  ( MintingPolicyHash
  , PlutusScript
  , Validator
  , ValidatorHash
  )
import Types.Transaction (TransactionOutput(TransactionOutput))
import Types.Value (Value)
import Undefined (undefined)

-- This is to replace ChainIndexTxOut, note we already have an OgmiosTxOut
-- in JsonWsp.OgmiosTxOut - that may be not be sufficient so consider the below.
-- If we can deserialise the below information from Ogmios, we should replace
-- the datatype JsonWsp.OgmiosTxOut.
data OgmiosTxOut
  = PublicKeyOgmiosTxOut
      { ogTxOutAddress :: Address
      , ogTxOutValue :: Value
      }
  | ScriptOgmiosTxOut
      { ogTxOutAddress :: Address
      , ogTxOutValidator :: Either ValidatorHash Validator
      , ogTxOutDatum :: Either DatumHash Datum
      , ogTxOutValue :: Value
      }

datumHash :: Datum -> DatumHash
datumHash = undefined

-- | Converts a transaction output from the Ogmios TxOut to the internal
-- transaction input.
--
-- Note that converting from 'OgmiosTxOut' to 'TxOutRef' and back to
-- 'OgmiosTxOut' loses precision ('Datum' and 'Validator' are changed to
-- 'DatumHash' and 'ValidatorHash' respectively)
toTxOut :: OgmiosTxOut -> TransactionOutput
toTxOut (PublicKeyOgmiosTxOut { ogTxOutAddress, ogTxOutValue }) =
  TransactionOutput
    { address: ogTxOutAddress
    , amount: ogTxOutValue
    , data_hash: Nothing
    }
toTxOut (ScriptOgmiosTxOut { ogTxOutAddress, ogTxOutDatum: Left datHash, ogTxOutValue }) =
  TransactionOutput
    { address: ogTxOutAddress
    , amount: ogTxOutValue
    , data_hash: Just datHash
    }
toTxOut (ScriptOgmiosTxOut { ogTxOutAddress, ogTxOutDatum: Right datum, ogTxOutValue }) =
  TransactionOutput
    { address: ogTxOutAddress
    , amount: ogTxOutValue
    , data_hash: Just $ datumHash datum
    }

newtype ScriptLookups (a :: Type) = ScriptLookups
  { slMPS :: Map MintingPolicyHash PlutusScript
  , slTxOutputs :: Map TxOutRef OgmiosTxOut
  }