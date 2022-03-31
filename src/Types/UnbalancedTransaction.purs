module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , PaymentPubKeyHash(..)
  , PubKeyHash(..)
  , ScriptOutput(..)
  , StakeKeyHash(..)
  , StakePubKeyHash(..)
  , TxOutRef
  , UnbalancedTx(..)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  -- , payPubKeyHash
  , payPubKeyHashAddress
  , payPubKeyHashBaseAddress
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  -- , pubKeyHash
  , pubKeyHashAddress
  , pubKeyHashBaseAddress
  , stakeKeyHashAddress
  , stakeKeyHashBaseAddress
  , stakePubKeyHashAddress
  , stakePubKeyHashBaseAddress
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , caseJsonObject
  , decodeJson
  , getField
  , JsonDecodeError(TypeMismatch)
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Types (Lens')
import Data.Map (Map, empty)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData)
import Serialization.Address
  ( Address
  , BaseAddress
  , NetworkId
  , baseAddressToAddress
  , pubKeyAddress
  )
import Serialization.Hash
  ( Ed25519KeyHash
  )
import ToData (class ToData)
import Types.Datum (DatumHash)
import Types.Transaction
  ( Transaction
  , TransactionInput
  , PublicKey(PublicKey)
  , Vkey(Vkey)
  , RequiredSigner(RequiredSigner)
  )
import Types.Scripts (ValidatorHash)
import Types.Value (Value)

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
  , datumHash :: DatumHash
  }

derive instance Newtype ScriptOutput _
derive instance Generic ScriptOutput _

instance Show ScriptOutput where
  show = genericShow

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance FromData PubKeyHash
derive newtype instance Ord PubKeyHash
derive newtype instance ToData PubKeyHash

instance Show PubKeyHash where
  show = genericShow

-- This is needed for `ApplyArgs`. Plutus has an `getPubKeyHash` field so don't
-- newtype derive.
instance DecodeJson PubKeyHash where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getPubKeyHash" >=> decodeJson >>> map PubKeyHash)

-- payPubKeyHash :: PaymentPubKey -> Maybe PaymentPubKeyHash
-- payPubKeyHash (PaymentPubKey pk) = wrap <$> pubKeyHash pk

-- pubKeyHash :: PublicKey -> Maybe PubKeyHash
-- pubKeyHash (PublicKey bech32) =
--   wrap <$> ed25519KeyHashFromBech32 bech32

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
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow

-- This is needed for `ApplyArgs`. Plutus has an `unPaymentPubKeyHash` field so
-- don't newtype derive.
instance DecodeJson PaymentPubKeyHash where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    ( flip getField "unPaymentPubKeyHash" >=>
        decodeJson >>> map PaymentPubKeyHash
    )

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
derive newtype instance FromData StakeKeyHash
derive newtype instance Ord StakeKeyHash
derive newtype instance ToData StakeKeyHash

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
derive newtype instance FromData StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash

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
derive instance Generic UnbalancedTx _

instance Show UnbalancedTx where
  show = genericShow

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
