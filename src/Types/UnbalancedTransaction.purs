module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , PaymentPubKeyHash(..)
  , PubKey(..)
  , PubKeyHash(..)
  , ScriptOutput(..)
  , TxOutputRef(..)
  , UnbalancedTx(..)
  , ValidatorHash(..)
  , scriptOutputToTxOutput
  , utxoIndexToUtxo
  ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Serialization.Address (addressFromBytes)
import Serialization.Hash (Ed25519KeyHash, ScriptHash, scriptHashToBytes)
import Types.ByteArray (ByteArray(..))
import Types.POSIXTimeRange (POSIXTimeRange)
import Types.Transaction (DataHash, Transaction, TransactionInput, TransactionOutput(..), Utxo)
import Types.Value (Value)

newtype PubKey = PubKey ByteArray

derive instance Newtype PubKey _
derive newtype instance Eq PubKey

newtype PaymentPubKey = PaymentPubKey PubKey

derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey

type ValidatorHash = ScriptHash

newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DataHash
  }

derive instance Newtype ScriptOutput _

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash

-- | Transaction inputs reference some other transaction's outputs.
type TxOutputRef = TransactionInput

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledeger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , requiredSignatories :: Map PaymentPubKeyHash (Maybe PaymentPubKey)
  , utxoIndex :: Map TxOutputRef ScriptOutput
  , validityTimeRange :: POSIXTimeRange
  }

derive instance Newtype UnbalancedTx _

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.OffChain.html#fromScriptOutput
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/src/Ledger.Tx.html#toTxOut
-- Combining these two into one function skipping Chain Index
-- | Converts a ScriptOutput to a TransactionOutput with potential failure
scriptOutputToTxOutput :: ScriptOutput -> Maybe TransactionOutput
scriptOutputToTxOutput (ScriptOutput { validatorHash, value, datumHash }) = do
  address <- validatorHash # scriptHashToBytes >>> addressFromBytes
  pure $ wrap { address, amount: value, data_hash: pure datumHash }

-- | Converts a utxoIndex from UnbalancedTx to Utxo with potential failure
utxoIndexToUtxo :: Map TxOutputRef ScriptOutput -> Maybe Utxo
utxoIndexToUtxo = map scriptOutputToTxOutput >>> sequence
