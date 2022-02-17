module Types.Transaction where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Serialization.Address (Address, NetworkId, RewardAddress, Slot)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Types.Value (Coin, Value)
import Serialization.Hash (Ed25519KeyHash)

-- note: these types are derived from the cardano-serialization-lib Sundae fork
-- the source of truth for these types should be that library and the
-- corresponding Rust types
newtype Transaction = Transaction
  { body :: TxBody
  , witness_set :: TransactionWitnessSet
  , is_valid :: Boolean
  , auxiliary_data :: Maybe AuxiliaryData
  }

derive instance newtypeTransaction :: Newtype Transaction _

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

derive instance newtypeTxBody :: Newtype TxBody _
derive newtype instance eqTxBody :: Eq TxBody

newtype ScriptDataHash = ScriptDataHash String

derive instance newtypeScriptDataHash :: Newtype ScriptDataHash _
derive newtype instance eqScriptDataHash :: Eq ScriptDataHash

newtype Mint = Mint Value

derive instance newtypeMint :: Newtype Mint _
derive newtype instance eqMint :: Eq Mint

newtype AuxiliaryDataHash = AuxiliaryDataHash String

derive instance newtypeAuxiliaryDataHash :: Newtype AuxiliaryDataHash _
derive newtype instance eqAuxiliaryDataHash :: Eq AuxiliaryDataHash

type Update =
  { proposed_protocol_parameter_updates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

newtype ProposedProtocolParameterUpdates = ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

derive instance newtypeProposedProtocolParameterUpdates :: Newtype ProposedProtocolParameterUpdates _

derive newtype instance eqProposedProtocolParameterUpdates :: Eq ProposedProtocolParameterUpdates

newtype GenesisHash = GenesisHash String

derive instance newtypeGenesisHash :: Newtype GenesisHash _
derive newtype instance eqGenesisHash :: Eq GenesisHash

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
  { mem :: BigInt
  , steps :: BigInt
  }

type SubCoin = UnitInterval

newtype Costmdls = Costmdls (Map Language CostModel)

derive instance newtypeCostmdls :: Newtype Costmdls _
derive newtype instance eqCostmdls :: Eq Costmdls

data Language = PlutusV1

derive instance eqLanguage :: Eq Language

newtype CostModel = CostModel (Array UInt)

derive instance newtypeCostModel :: Newtype CostModel _
derive newtype instance eqCostModel :: Eq CostModel

type ProtocolVersion =
  { major :: UInt
  , minor :: UInt
  }

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _
derive newtype instance eqNonce :: Eq Nonce

type UnitInterval =
  { numerator :: BigInt
  , denominator :: BigInt
  }

newtype Epoch = Epoch UInt

derive instance newtypeEpoch :: Newtype Epoch _
derive newtype instance eqEpoch :: Eq Epoch

data Certificate
  = StakeRegistration
  | StakeDeregistration
  | StakeDelegation
  | PoolRegistration
  | PoolRetirement
  | GenesisKeyDelegation
  | MoveInstantaneousRewardsCert

derive instance eqCertificate :: Eq Certificate

newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Maybe (Array Vkeywitness)
  , native_scripts :: Maybe (Array NativeScript)
  , bootstraps :: Maybe (Array BootstrapWitness)
  , plutus_scripts :: Maybe (Array PlutusScript)
  , plutus_data :: Maybe (Array PlutusData)
  , redeemers :: Maybe (Array Redeemer)
  }

derive instance Generic TransactionWitnessSet _
derive instance Newtype TransactionWitnessSet _
derive newtype instance Eq TransactionWitnessSet

instance Show TransactionWitnessSet where
  show = genericShow

emptyTransactionWitnessSet :: TransactionWitnessSet
emptyTransactionWitnessSet = TransactionWitnessSet
  { vkeys: Nothing
  , native_scripts: Nothing
  , bootstraps: Nothing
  , plutus_scripts: Nothing
  , plutus_data: Nothing
  , redeemers: Nothing
  }

type BootstrapWitness =
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chain_code :: ByteArray
  , attributes :: ByteArray
  }

newtype RequiredSigner = RequiredSigner String

derive instance newtypeRequiredSigner :: Newtype RequiredSigner _
derive newtype instance eqRequiredSigner :: Eq RequiredSigner

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

derive instance Generic Vkeywitness _
derive newtype instance Eq Vkeywitness

instance Show Vkeywitness where
  show = genericShow

newtype Vkey = Vkey PublicKey

derive instance Generic Vkey _
derive instance Newtype Vkey _
derive newtype instance Eq Vkey

instance Show Vkey where
  show = genericShow

newtype PublicKey = PublicKey Bech32String

derive instance Generic PublicKey _
derive instance Newtype PublicKey _
derive newtype instance Eq PublicKey

instance Show PublicKey where
  show = genericShow

newtype Ed25519Signature = Ed25519Signature Bech32String

derive instance Generic Ed25519Signature _
derive newtype instance Eq Ed25519Signature

instance Show Ed25519Signature where
  show = genericShow

newtype PlutusScript = PlutusScript ByteArray

derive instance newtypePlutusScript :: Newtype PlutusScript _
derive newtype instance eqPlutusScript :: Eq PlutusScript
derive instance Generic PlutusScript _

instance Show PlutusScript where
  show = genericShow

newtype PlutusData = PlutusData ByteArray

derive instance Generic PlutusData _
derive newtype instance Eq PlutusData
derive instance Newtype PlutusData _

instance Show PlutusData where
  show = genericShow

newtype Redeemer = Redeemer
  { tag :: RedeemerTag
  , index :: BigInt
  , data :: PlutusData
  , ex_units :: ExUnits
  }

derive instance Generic Redeemer _
derive newtype instance Eq Redeemer

instance Show Redeemer where
  show = genericShow

type AuxiliaryData =
  { metadata :: Maybe GeneralTransactionMetadata
  , native_scripts :: Maybe (Array NativeScript)
  , plutus_scripts :: Maybe (Array PlutusScript)
  }

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata (HashMap TransactionMetadatumLabel TransactionMetadatum)

derive instance newtypeGeneralTransactionMetadata :: Newtype GeneralTransactionMetadata _

derive newtype instance eqGeneralTransactionMetadata :: Eq GeneralTransactionMetadata

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt

derive instance newtypeTransactionMetadatumLabel :: Newtype TransactionMetadatumLabel _

derive newtype instance eqTransactionMetadatumLabel :: Eq TransactionMetadatumLabel

data TransactionMetadatum
  = MetadataMap (HashMap TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int
  | Bytes ByteArray
  | Text String

derive instance eqTransactionMetadatum :: Eq TransactionMetadatum

data NativeScript
  = ScriptPubkey Ed25519KeyHash
  | ScriptAll (Array NativeScript)
  | ScriptAny (Array NativeScript)
  | ScriptNOfK Int (Array NativeScript)
  | TimelockStart Slot
  | TimelockExpiry Slot

derive instance eqNativeScript :: Eq NativeScript
derive instance Generic NativeScript _

instance Show NativeScript where
  show x = genericShow x

newtype TransactionInput = TransactionInput
  { transaction_id :: TransactionHash
  , index :: UInt
  }

derive instance newtypeTransactionInput :: Newtype TransactionInput _
derive instance genericTransactionInput :: Generic TransactionInput _
derive newtype instance eqTransactionInput :: Eq TransactionInput
derive newtype instance ordTransactionInput :: Ord TransactionInput

instance showTransactionInput :: Show TransactionInput where
  show = genericShow

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , data_hash :: Maybe DataHash
  }

derive instance genericTransactionOutput :: Generic TransactionOutput _
derive instance newtypeTransactionOutput :: Newtype TransactionOutput _
derive newtype instance eqTransactionOutput :: Eq TransactionOutput

instance showTransactionOutput :: Show TransactionOutput where
  show = genericShow

newtype UtxoM = UtxoM Utxo

derive instance newtypeUtxoM :: Newtype UtxoM _
derive newtype instance showUtxoM :: Show UtxoM

type Utxo = Map TransactionInput TransactionOutput

newtype TransactionHash = TransactionHash ByteArray

derive newtype instance Eq TransactionHash
derive newtype instance Ord TransactionHash
derive instance Generic TransactionHash _
derive instance Newtype TransactionHash _

instance Show TransactionHash where
  show = genericShow

newtype DataHash = DataHash ByteArray

derive instance Generic DataHash _
derive instance newtypeDataHash :: Newtype DataHash _
derive newtype instance eqDataHash :: Eq DataHash
derive newtype instance ordDataHash :: Ord DataHash

instance Show DataHash where
  show = genericShow

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
