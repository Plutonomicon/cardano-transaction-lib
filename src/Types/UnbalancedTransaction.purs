module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , ScriptOutput(..)
  , UnbalancedTx(..)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  ) where

import Prelude

import Aeson(class EncodeAeson, encodeAeson, encodeAeson')
import Aeson.Encode(dictionary)
import Cardano.Types.Transaction
  ( Transaction
  , PublicKey(PublicKey)
  , Vkey(Vkey)
  , RequiredSigner(RequiredSigner)
  )
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Types (Lens')
import Data.Map (Map, empty)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Op(Op(Op))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Serialization
  ( publicKeyFromBech32
  , publicKeyHash
  )
import Types.Datum (DataHash)
import Types.Transaction (TransactionInput)
import Types.Scripts (ValidatorHash)
import Cardano.Types.Value (Value)

-- Plutus has a type called `PubKey` which we replace with `PublicKey`
newtype PaymentPubKey = PaymentPubKey PublicKey

derive instance Generic PaymentPubKey _
derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey
derive newtype instance Ord PaymentPubKey

instance Show PaymentPubKey where
  show = genericShow

-- Plutus uses this type in recent revs but wonder if we even need it.
newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DataHash
  }

derive instance Newtype ScriptOutput _
derive instance Generic ScriptOutput _
derive newtype instance Eq ScriptOutput
derive newtype instance EncodeAeson ScriptOutput

instance Show ScriptOutput where
  show = genericShow

payPubKeyVkey :: PaymentPubKey -> Vkey
payPubKeyVkey (PaymentPubKey pk) = Vkey pk

payPubKeyRequiredSigner :: PaymentPubKey -> Maybe RequiredSigner
payPubKeyRequiredSigner (PaymentPubKey (PublicKey bech32)) =
  RequiredSigner <<< publicKeyHash <$> publicKeyFromBech32 bech32

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , utxoIndex :: Map TransactionInput ScriptOutput
  }

derive instance Newtype UnbalancedTx _
derive instance Generic UnbalancedTx _
derive newtype instance Eq UnbalancedTx

instance Show UnbalancedTx where
  show = genericShow

instance EncodeAeson UnbalancedTx where
    encodeAeson' (UnbalancedTx r)  = encodeAeson' $ r {
        utxoIndex = encodeMap r.utxoIndex
    }
        where (Op encodeMap) = dictionary (Op encodeAeson) (Op encodeAeson)

_transaction :: Lens' UnbalancedTx Transaction
_transaction = lens'
  \(UnbalancedTx rec@{ transaction }) ->
    Tuple
      transaction
      \tx -> UnbalancedTx rec { transaction = tx }

_utxoIndex :: Lens' UnbalancedTx (Map TransactionInput ScriptOutput)
_utxoIndex = lens'
  \(UnbalancedTx rec@{ utxoIndex }) ->
    Tuple
      utxoIndex
      \utxoIx -> UnbalancedTx rec { utxoIndex = utxoIx }

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx { transaction: mempty, utxoIndex: empty }
