module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , PaymentPubKeyHash(..)
  , PubKeyHash(..)
  , ScriptOutput(..)
  , StakeKeyHash(..)
  , StakePubKeyHash(..)
  , TxOutRef(..)
  , UnbalancedTx(..)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  , payPubKeyHash
  , payPubKeyHashAddress
  , payPubKeyHashBaseAddress
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  , pubKeyHash
  , pubKeyHashAddress
  , pubKeyHashBaseAddress
  , scriptOutputToTxOutput
  , stakeKeyHashAddress
  , stakeKeyHashBaseAddress
  , stakePubKeyHashAddress
  , stakePubKeyHashBaseAddress
  , utxoIndexToUtxo
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Types (Lens')
import Data.Map (Map, empty)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Serialization.Address
  ( Address
  , BaseAddress
  , NetworkId
  , addressFromBytes
  , baseAddressToAddress
  , pubKeyAddress
  )
import Serialization.Hash
  ( Ed25519KeyHash
  , ed25519KeyHashFromBech32
  , scriptHashToBytes
  )
import Types.Datum (DatumHash)
import Types.Scripts (ValidatorHash)
import Types.Transaction
  ( Transaction
  , TransactionInput
  , TransactionOutput
  , Utxo
  , PublicKey(PublicKey)
  , Vkey(Vkey)
  , RequiredSigner(RequiredSigner)
  )
import Types.Value (Value)

-- Plutus has a type called `PubKey` which we replace with `PublicKey`
newtype PaymentPubKey = PaymentPubKey PublicKey

derive instance Generic PaymentPubKey _
derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey

instance Show PaymentPubKey where
  show = genericShow

newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DatumHash
  }

derive instance Newtype ScriptOutput _

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance Ord PubKeyHash

instance Show PubKeyHash where
  show = genericShow

payPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
payPubKeyHash (PaymentPubKey pk) = pubKeyHash pk

-- Is this safe? If we start with a PubKey, we should be guaranteed a hash
-- (even if via ByteArray)
pubKeyHash :: PublicKey -> PaymentPubKeyHash
pubKeyHash (PublicKey bech32) =
  wrap $ wrap $ unsafePartial fromJust $ ed25519KeyHashFromBech32 bech32

payPubKeyVkey :: PaymentPubKey -> Vkey
payPubKeyVkey (PaymentPubKey pk) = Vkey pk

payPubKeyRequiredSigner :: PaymentPubKey -> RequiredSigner
payPubKeyRequiredSigner pk = RequiredSigner $ payPubKeyVkey pk

ed25519BaseAddress
  :: forall (n :: Type)
   . Newtype n Ed25519KeyHash
  => NetworkId
  -> n
  -> BaseAddress
ed25519BaseAddress networkId n = pubKeyAddress networkId (unwrap n)

pubKeyHashBaseAddress :: NetworkId -> PubKeyHash -> BaseAddress
pubKeyHashBaseAddress networkId = ed25519BaseAddress networkId

pubKeyHashAddress :: NetworkId -> PubKeyHash -> Address
pubKeyHashAddress networkId =
  baseAddressToAddress <<< pubKeyHashBaseAddress networkId

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow

payPubKeyHashBaseAddress :: NetworkId -> PaymentPubKeyHash -> BaseAddress
payPubKeyHashBaseAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashBaseAddress networkId pkh

-- Note, Plutus has a function pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Address
-- but we don't appear to require an optional StakePubKeyHash.
payPubKeyHashAddress :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashAddress networkId pkh

newtype StakeKeyHash = StakeKeyHash Ed25519KeyHash

derive instance Generic StakeKeyHash _
derive instance Newtype StakeKeyHash _
derive newtype instance Eq StakeKeyHash
derive newtype instance Ord StakeKeyHash

instance Show StakeKeyHash where
  show = genericShow

stakeKeyHashBaseAddress :: NetworkId -> StakeKeyHash -> BaseAddress
stakeKeyHashBaseAddress networkId = ed25519BaseAddress networkId

stakeKeyHashAddress :: NetworkId -> StakeKeyHash -> Address
stakeKeyHashAddress networkId =
  baseAddressToAddress <<< stakeKeyHashBaseAddress networkId

newtype StakePubKeyHash = StakePubKeyHash StakeKeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance Ord StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

stakePubKeyHashBaseAddress :: NetworkId -> StakePubKeyHash -> BaseAddress
stakePubKeyHashBaseAddress networkId (StakePubKeyHash skh) =
  stakeKeyHashBaseAddress networkId skh

-- Note, Plutus has a function pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Address
-- but we don't appear to require an optional StakePubKeyHash.
stakePubKeyHashAddress :: NetworkId -> StakePubKeyHash -> Address
stakePubKeyHashAddress networkId (StakePubKeyHash skh) =
  stakeKeyHashAddress networkId skh

-- Use Plutus' name to assist with copy & paste from Haskell to Purescript.
-- | Transaction inputs reference some other transaction's outputs.
type TxOutRef = TransactionInput

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledeger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , utxoIndex :: Map TxOutRef ScriptOutput
  }

derive instance Newtype UnbalancedTx _

_transaction :: Lens' UnbalancedTx Transaction
_transaction = lens'
  \(UnbalancedTx rec@{ transaction }) ->
    Tuple
      transaction
      \tx -> UnbalancedTx rec { transaction = tx }

_utxoIndex :: Lens' UnbalancedTx (Map TxOutRef ScriptOutput)
_utxoIndex = lens'
  \(UnbalancedTx rec@{ utxoIndex }) ->
    Tuple
      utxoIndex
      \utxoIx -> UnbalancedTx rec { utxoIndex = utxoIx }

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx { transaction: mempty, utxoIndex: empty }

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.OffChain.html#fromScriptOutput
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/src/Ledger.Tx.html#toTxOut
-- Combining these two into one function skipping Chain Index
-- | Converts a `ScriptOutput` to a `TransactionOutput` with potential failure
scriptOutputToTxOutput :: ScriptOutput -> Maybe TransactionOutput
scriptOutputToTxOutput (ScriptOutput { validatorHash, value, datumHash }) = do
  address <- validatorHash # unwrap # scriptHashToBytes >>> addressFromBytes
  pure $ wrap { address, amount: value, data_hash: pure datumHash }

-- | Converts a utxoIndex from `UnbalancedTx` to `Utxo` with potential failure
utxoIndexToUtxo :: Map TxOutRef ScriptOutput -> Maybe Utxo
utxoIndexToUtxo = map scriptOutputToTxOutput >>> sequence
