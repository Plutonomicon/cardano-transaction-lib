module Types.Transaction where

import Prelude
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)

-- note: these types are derived from the cardano-serialization-lib Sundae fork
-- the source of truth for these types should be that library and the
-- corresponding Rust types
newtype Transaction = Transaction
  { body :: TxBody
  , witness_set :: TransactionWitnessSet
  , is_valid :: Boolean
  , auxiliary_data :: Maybe AuxiliaryData
  }

newtype TxBody = TxBody
  { inputs :: Array TransactionInput
  , outputs :: Array TransactionOutput
  , fee :: Coin
  , ttl :: Maybe Slot
  , certs :: Maybe (Array Certificate)
  , withdrawals :: Maybe (Map RewardAddress Coin)
  , update :: Maybe Update
  , auxiliary_data_hash :: Maybe AuxiliaryDataHash
  , validity_start_interval :: Maybe Slot
  , mint :: Maybe Mint
  , script_data_hash :: Maybe ScriptDataHash
  , collateral :: Maybe (Array TransactionInput)
  , required_signers :: Maybe (Array RequiredSigner)
  , network_id :: Maybe NetworkId
  }

newtype ScriptDataHash = ScriptDataHash String

newtype Mint = Mint Value

newtype AuxiliaryDataHash = AuxiliaryDataHash String

type Update =
  { proposed_protocol_parameter_updates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

newtype ProposedProtocolParameterUpdates = ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

newtype GenesisHash = GenesisHash String

type ProtocolParamUpdate =
  { minfee_a :: Maybe Coin
  , minfee_b :: Maybe Coin
  , max_block_body_size :: Maybe UInt
  , max_tx_size :: Maybe UInt
  , max_block_header_size :: Maybe UInt
  , key_deposit :: Maybe Coin
  , pool_deposit :: Maybe Coin
  , max_epoch :: Maybe Epoch
  , n_opt :: Maybe UInt
  , pool_pledge_influence :: Maybe Rational
  , expansion_rate :: Maybe UnitInterval
  , treasury_growth_rate :: Maybe UnitInterval
  , d :: Maybe UnitInterval
  , extra_entropy :: Maybe Nonce
  , protocol_version :: Maybe (Array ProtocolVersion)
  , min_pool_cost :: Maybe Coin
  , ada_per_utxo_byte :: Maybe Coin
  , cost_models :: Maybe Costmdls
  , execution_costs :: Maybe ExUnitPrices
  , max_tx_ex_units :: Maybe ExUnits
  , max_block_ex_units :: Maybe ExUnits
  , max_value_size :: Maybe UInt
  }

type ExUnitPrices =
  { mem_price :: SubCoin
  , step_price :: SubCoin
  }

type ExUnits =
  { mem :: BigInt.BigInt
  , steps :: BigInt.BigInt
  }

type SubCoin = UnitInterval

type RewardAddress =
  { network :: UInt
  , payment :: StakeCredential
  }

data StakeCredential
  = StakeCredentialKey Ed25519KeyHash
  | StakeCredentialScript ScriptHash

derive instance Generic StakeCredential _
derive instance Eq StakeCredential

instance Show StakeCredential where
  show = genericShow

newtype Ed25519KeyHash = Ed25519KeyHash ByteArray

derive newtype instance Eq Ed25519KeyHash
derive newtype instance Ord Ed25519KeyHash
derive instance Generic Ed25519KeyHash _

instance Show Ed25519KeyHash where
  show = genericShow

newtype ScriptHash = ScriptHash ByteArray

derive instance Generic ScriptHash _
derive newtype instance Eq ScriptHash

instance Show ScriptHash where
  show = genericShow

newtype Costmdls = Costmdls (Map Language CostModel)

data Language = PlutusV1

newtype CostModel = CostModel (Array UInt)

type ProtocolVersion =
  { major :: UInt
  , minor :: UInt
  }

newtype Nonce = Nonce String

type UnitInterval =
  { numerator :: BigInt.BigInt
  , denominator :: BigInt.BigInt
  }

newtype Epoch = Epoch UInt

data Certificate
  = StakeRegistration
  | StakeDeregistration
  | StakeDelegation
  | PoolRegistration
  | PoolRetirement
  | GenesisKeyDelegation
  | MoveInstantaneousRewardsCert

newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Maybe (Array Vkeywitness)
  , native_scripts :: Maybe (Array NativeScript)
  , bootstraps :: Maybe (Array BootstrapWitness)
  , plutus_scripts :: Maybe (Array PlutusScript)
  , plutus_data :: Maybe (Array PlutusData)
  , redeemers :: Maybe (Array Redeemer)
  }

type BootstrapWitness =
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chain_code :: ByteArray
  , attributes :: ByteArray
  }

data NetworkId
  = Mainnet
  | Testnet

newtype RequiredSigner = RequiredSigner String

newtype CurrencySymbol = CurrencySymbol ByteArray

derive instance Generic CurrencySymbol _
derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol

instance Show CurrencySymbol where
  show = genericShow

newtype TokenName = TokenName ByteArray

derive instance Generic TokenName _
derive newtype instance eqTokenName :: Eq TokenName
derive newtype instance ordTokenName :: Ord TokenName

instance Show TokenName where
  show = genericShow

-- | In Plutus, Ada is is stored inside the map (with currency symbol and token
-- | name being empty bytestrings). cardano-serialization-lib makes semantic
-- | distinction between native tokens and Ada, and we follow this convention.
data Value = Value Coin (Map CurrencySymbol (Map TokenName BigInt.BigInt))

derive instance Generic Value _

instance Show Value where
  show = genericShow

newtype Bech32 = Bech32 String

derive newtype instance eqBech32 :: Eq Bech32
derive instance Generic Bech32 _
derive instance Newtype Bech32 _
instance Show Bech32 where
  show = genericShow

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

newtype Vkey = Vkey PublicKey

derive instance Generic Vkey _
derive instance Newtype Vkey _
instance Show Vkey where
  show = genericShow

newtype PublicKey = PublicKey Bech32

derive instance Generic PublicKey _
derive instance Newtype PublicKey _
instance Show PublicKey where
  show = genericShow

newtype Ed25519Signature = Ed25519Signature Bech32

newtype PlutusScript = PlutusScript ByteArray

newtype PlutusData = PlutusData String

newtype Redeemer = Redeemer
  { tag :: RedeemerTag
  , index :: BigInt.BigInt
  , data :: PlutusData
  , ex_units :: (MemExUnits /\ CpuExUnits)
  }

newtype MemExUnits = MemExUnits BigInt.BigInt

newtype CpuExUnits = CpuExUnits BigInt.BigInt

type AuxiliaryData =
  { metadata :: Maybe GeneralTransactionMetadata
  , native_scripts :: Maybe (Array NativeScript)
  , plutus_scripts :: Maybe (Array PlutusScript)
  }

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata (HashMap TransactionMetadatumLabel TransactionMetadatum)

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt.BigInt

data TransactionMetadatum
  = MetadataMap (HashMap TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int
  | Bytes ByteArray
  | Text String

data NativeScript
  = ScriptPubkey
  | ScriptAll
  | ScriptAny
  | ScriptNOfK
  | TimelockStart
  | TimelockExpiry

newtype TransactionInput = TransactionInput
  { transaction_id :: TransactionHash
  , index :: UInt
  }

derive newtype instance Eq TransactionInput
derive newtype instance Ord TransactionInput
derive instance Generic TransactionInput _
derive instance Newtype TransactionInput _

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , data_hash :: Maybe DataHash
  }

newtype TransactionHash = TransactionHash ByteArray

derive newtype instance Eq TransactionHash
derive newtype instance Ord TransactionHash
derive instance Generic TransactionHash _
derive instance Newtype TransactionHash _

instance Show TransactionHash where
  show = genericShow

newtype DataHash = DataHash ByteArray

derive instance Generic DataHash _
derive instance Newtype DataHash _

instance Show DataHash where
  show = genericShow

newtype Coin = Coin BigInt.BigInt

derive instance Newtype Coin _
derive instance Generic Coin _

instance Show Coin where
  show = genericShow

newtype Slot = Slot BigInt.BigInt

newtype Address = Address
  { "AddrType" :: BaseAddress
  }

derive instance Newtype Address _
derive instance Generic Address _
derive newtype instance Eq Address

instance Show Address where
  show = genericShow

newtype BaseAddress = BaseAddress
  { network :: UInt -- UInt8
  , stake :: StakeCredential
  , payment :: PaymentCredential
  }

derive instance Newtype BaseAddress _
derive instance Generic BaseAddress _
derive newtype instance Eq BaseAddress

instance Show BaseAddress where
  show = genericShow

data PaymentCredential
  = PaymentCredentialKey Ed25519KeyHash
  | PaymentCredentialScript ScriptHash

derive instance Generic PaymentCredential _
derive instance Eq PaymentCredential

instance Show PaymentCredential where
  show = genericShow

-- Addresspub struct Address(AddrType);
-- AddrType
-- enum AddrType {
-- Base(BaseAddress),
-- Ptr(PointerAddress),
-- Enterprise(EnterpriseAddress),
-- Reward(RewardAddress),
-- Byron(ByronAddress),
-- }
-- pub struct BaseAddress {
-- network: u8,
-- payment: StakeCredential,
-- stake: StakeCredential,
-- }
-- pub struct StakeCredential(StakeCredType);
-- Both of these are strings:
-- enum StakeCredType {
-- Key(Ed25519KeyHash),
-- Script(ScriptHash),
-- }

-- Option<Certificates>,
-- these are the constructors, but this will generally be an Empty Option in our initial efforts
-- StakeRegistration(StakeRegistration),
-- StakeDeregistration(StakeDeregistration),
-- StakeDelegation(StakeDelegation),
-- PoolRegistration(PoolRegistration),
-- PoolRetirement(PoolRetirement),
-- GenesisKeyDelegation(GenesisKeyDelegation),
-- MoveInstantaneousRewardsCert(MoveInstantaneousRewardsCert),

-- Option<Withdrawals>,
-- also mainly empty to start
-- pub struct RewardAddress {
-- network: u8,
-- payment: StakeCredential,
-- Option<Update>,
-- again this will be empty
-- pub struct Update {
-- proposed_protocol_parameter_updates: ProposedProtocolParameterUpdates,
-- epoch: Epoch,
-- }
-- Option<AuxiliaryDataHash> -- String
-- Option<Slot> -- Intege
-- Option<Mint> -- BTreeMap PolicyId MintAssets
-- MintAssets :: BTreeMap AssetName Int32
-- Option<ScriptDataHash> -- String
-- Option<TransactionInputs> -- for collateral
-- Option<RequiredSigners> -- Array String (Ed25519 signatures)
-- Option<NetworkId>
--  { networkIdKind :: Testnet | Mainnet }
