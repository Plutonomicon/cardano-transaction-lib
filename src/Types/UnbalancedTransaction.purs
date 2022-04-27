module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , PaymentPubKeyHash(..)
  , ScriptOutput(..)
  , StakePubKeyHash(..)
  , TxOutRef
  , UnbalancedTx(..)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  , payPubKeyHashBaseAddress
  , payPubKeyHashRewardAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyRequiredSigner
  , payPubKeyVkey
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , pubKeyHashRewardAddress
  , stakePubKeyHashRewardAddress
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
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData)
import Serialization
  ( publicKeyFromBech32
  , publicKeyHash
  )
import Serialization.Address
  ( Address
  , EnterpriseAddress
  , RewardAddress
  , NetworkId
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  , pubKeyAddress
  , rewardAddress
  , rewardAddressToAddress
  )
import Serialization.Hash
  ( Ed25519KeyHash
  )
import ToData (class ToData)
import Types.Datum (DatumHash)
import Types.PubKeyHash (PubKeyHash)
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
derive newtype instance Eq ScriptOutput

instance Show ScriptOutput where
  show = genericShow

payPubKeyVkey :: PaymentPubKey -> Vkey
payPubKeyVkey (PaymentPubKey pk) = Vkey pk

payPubKeyRequiredSigner :: PaymentPubKey -> Maybe RequiredSigner
payPubKeyRequiredSigner (PaymentPubKey (PublicKey bech32)) =
  RequiredSigner <<< publicKeyHash <$> publicKeyFromBech32 bech32

ed25519EnterpriseAddress
  :: forall (n :: Type)
   . Newtype n Ed25519KeyHash
  => NetworkId
  -> n
  -> EnterpriseAddress
ed25519EnterpriseAddress network pkh =
  enterpriseAddress
    { network
    , paymentCred: keyHashCredential (unwrap pkh)
    }

ed25519RewardAddress
  :: forall (n :: Type)
   . Newtype n Ed25519KeyHash
  => NetworkId
  -> n
  -> RewardAddress
ed25519RewardAddress network skh =
  rewardAddress
    { network
    , paymentCred: keyHashCredential (unwrap skh)
    }

pubKeyHashBaseAddress :: NetworkId -> PubKeyHash -> StakePubKeyHash -> Address
pubKeyHashBaseAddress networkId pkh skh =
  baseAddressToAddress $ pubKeyAddress networkId (unwrap pkh) (unwrap skh)

pubKeyHashRewardAddress :: NetworkId -> PubKeyHash -> Address
pubKeyHashRewardAddress networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId

pubKeyHashEnterpriseAddress :: NetworkId -> PubKeyHash -> Address
pubKeyHashEnterpriseAddress networkId =
  enterpriseAddressToAddress <<< ed25519EnterpriseAddress networkId

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

newtype StakePubKeyHash = StakePubKeyHash Ed25519KeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance FromData StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

payPubKeyHashRewardAddress :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashRewardAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashRewardAddress networkId pkh

payPubKeyHashBaseAddress
  :: NetworkId -> PaymentPubKeyHash -> StakePubKeyHash -> Address
payPubKeyHashBaseAddress networkId (PaymentPubKeyHash pkh) skh =
  pubKeyHashBaseAddress networkId pkh skh

payPubKeyHashEnterpriseAddress :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashEnterpriseAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashEnterpriseAddress networkId pkh

stakePubKeyHashRewardAddress :: NetworkId -> StakePubKeyHash -> Address
stakePubKeyHashRewardAddress networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId

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
derive newtype instance Eq UnbalancedTx

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
