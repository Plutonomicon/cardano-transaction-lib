module Types.UnbalancedTransaction where

import Data.BigInt (BigInt)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude
import Types.ByteArray (ByteArray(..))
import Types.POSIXTimeRange (POSIXTimeRange)
import Types.Transaction (Ed25519KeyHash(..), Transaction, TransactionHash, Value)

newtype PubKey = PubKey ByteArray

derive instance Newtype PubKey _
derive newtype instance Eq PubKey

newtype PaymentPubKey = PaymentPubKey PubKey

derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey

newtype TxOutRef = TxOutRef
  { id :: TransactionHash
  , index :: BigInt
  -- ^ Index into the referenced transaction's outputs
  }

derive instance Newtype TxOutRef _
derive newtype instance Eq TxOutRef

newtype ValidatorHash = ValidatorHash ByteArray

derive instance Newtype ValidatorHash _
derive newtype instance Eq ValidatorHash

newtype DatumHash = DatumHash ByteArray

derive instance Newtype DatumHash _
derive newtype instance Eq DatumHash

newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DatumHash
  }

derive instance Newtype ScriptOutput _

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash

newtype PaymentPubKeyHash = PaymentPubKeyHasb PubKeyHash

derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledeger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , requiredSignatories :: Map PaymentPubKeyHash (Maybe PaymentPubKey)
  , utxoIndex :: Map TxOutRef ScriptOutput
  , validityTimeRange :: POSIXTimeRange
  }

derive instance Newtype UnbalancedTx _
