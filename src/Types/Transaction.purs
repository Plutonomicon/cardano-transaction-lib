module Types.Transaction where

import Prelude
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Map (Map)
import Data.Maybe (Maybe)
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
  = Key Ed25519KeyHash
  | Script ScriptHash

newtype Ed25519KeyHash = Ed25519KeyHash String

newtype ScriptHash = ScriptHash String

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

derive instance genericCurrencySymbol :: Generic CurrencySymbol _
derive newtype instance eqCurrencySymbol :: Eq CurrencySymbol
derive newtype instance ordCurrencySymbol :: Ord CurrencySymbol

instance showCurrencySymbol :: Show CurrencySymbol where
  show = genericShow

newtype TokenName = TokenName ByteArray

derive instance genericTokenName :: Generic TokenName _
derive newtype instance eqTokenName :: Eq TokenName
derive newtype instance ordTokenName :: Ord TokenName

instance showTokenName :: Show TokenName where
  show = genericShow

newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt.BigInt))

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show = genericShow

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

newtype Vkey = Vkey String -- (bech32)

newtype Ed25519Signature = Ed25519Signature String -- (bech32)

newtype PlutusScript = PlutusScript String

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

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , data_hash :: Maybe DataHash
  }

newtype TransactionHash = TransactionHash ByteArray

derive instance genericTransactionHash :: Generic TransactionHash _

instance showTransactionHash :: Show TransactionHash where
  show = genericShow

newtype DataHash = DataHash ByteArray

derive instance genericDataHash :: Generic DataHash _

instance showDataHash :: Show DataHash where
  show = genericShow

newtype Coin = Coin BigInt.BigInt

newtype Slot = Slot BigInt.BigInt

newtype Address = Address
  { "AddrType" :: BaseAddress
  }

newtype BaseAddress = BaseAddress
  { network :: UInt
  , -- UInt8
    stake :: Credential
  , payment :: Credential
  }

newtype Credential = Credential ByteArray

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
