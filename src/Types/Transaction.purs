module Types.Transaction where

import Prelude
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))

import Types.Value (Value)

newtype Transaction = Transaction {
  body :: TxBody,
  witness_set :: TransactionWitnessSet,
  is_valid :: Boolean,
  auxiliary_data :: Maybe AuxiliaryData
}
derive instance newtypeTransaction :: Newtype Transaction _

newtype TxBody = TxBody
  { inputs :: Array TransactionInput,
    outputs :: Array TransactionOutput,
    fee :: Coin,
    ttl :: Maybe Slot,
    certs :: Maybe Unit, -- Certificates,
    withdrawals :: Maybe Unit, -- Withdrawals,
    update :: Maybe Unit, -- Update,
    auxiliary_data_hash :: Maybe String, -- AuxiliaryDataHash, - script hashes
    validity_start_interval :: Maybe Slot,
    mint :: Maybe Value, -- Mint
    script_data_hash :: Maybe String, -- ScriptDataHash,
    collateral :: Maybe (Array TransactionInput),
    required_signers :: Maybe (Array RequiredSigner),
    network_id :: Maybe NetworkId
  }
derive instance newtypeTxBody :: Newtype TxBody _
derive instance eqTxBody :: Eq TxBody

newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Maybe (Array Vkeywitness),
    native_scripts :: Maybe Unit, -- NativeScripts,
    bootstraps :: Maybe Unit, -- BootstrapWitnesses,
    plutus_scripts :: Maybe (Array PlutusScript),
    plutus_data :: Maybe (Array PlutusData),
    redeemers :: Maybe (Array Redeemer)
  }

newtype NetworkId = NetworkId Int
derive instance eqNetworkId :: Eq NetworkId

newtype RequiredSigner = RequiredSigner String
derive instance eqRequiredSigner :: Eq RequiredSigner

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

newtype Vkey = Vkey String -- (bech32)

newtype Ed25519Signature = Ed25519Signature String -- (bech32)

newtype PlutusScript = PlutusScript String

newtype PlutusData = PlutusData String
-- TODO - we need a capability to encode/decode Datum from/to serialized format
-- see `makeIsDataIndexed`

newtype Redeemer = Redeemer
  { tag :: RedeemerTag, -- ScriptPurpose: 'spending' 'minting' etc
    index :: BigInt,
    data :: PlutusData,
    ex_units :: (MemExUnits /\ CpuExUnits)
  }

newtype MemExUnits = MemExUnits BigInt

newtype CpuExUnits = CpuExUnits BigInt

data RedeemerTag = Spend | Mint | Cert | Reward

type AuxiliaryData = Unit -- this is big and weird in serialization-lib

newtype TransactionInput = TransactionInput
  { transaction_id :: String, -- TransactionHash
    index :: BigInt -- u32 TransactionIndex
  }
derive instance eqTransactionInput :: Eq TransactionInput
derive instance genericTransactionInput :: Generic TransactionInput _
derive instance ordTransactionInput :: Ord TransactionInput

instance showTransactionInput :: Show TransactionInput where
  show = genericShow

newtype TransactionOutput = TransactionOutput
  { address :: Address,
    amount :: Value,
    data_hash :: Maybe String -- DataHash>,
  }
derive instance eqTransactionOutput :: Eq TransactionOutput
derive instance genericTransactionOutput :: Generic TransactionOutput _
derive instance newtypeTransactionOutput :: Newtype TransactionOutput _

instance showTransactionOutput :: Show TransactionOutput where
  show = genericShow

newtype UtxoM = UtxoM Utxo
derive instance newtypeUtxoM :: Newtype UtxoM _
derive newtype instance showUtxoM :: Show UtxoM

type Utxo = Map TransactionInput TransactionOutput

newtype Coin = Coin BigInt
derive instance eqCoin :: Eq Coin
derive instance newtypeCoin :: Newtype Coin _

newtype Slot = Slot BigInt
derive instance eqSlot :: Eq Slot

newtype Address = Address
  { "AddrType" :: BaseAddress
  }
derive instance eqAddress :: Eq Address
derive instance genericAddress :: Generic Address _
derive instance ordAddress :: Ord Address
derive instance newtypeAddress :: Newtype Address _

instance showAddress :: Show Address where
  show = genericShow

newtype BaseAddress = BaseAddress
  { network :: Int, -- u8,
    stake :: Credential,
    payment :: Credential
  }
derive instance eqBaseAddress :: Eq BaseAddress
derive instance genericBaseAddress :: Generic BaseAddress _
derive instance ordBaseAddress :: Ord BaseAddress
derive instance newtypeBaseAddress :: Newtype BaseAddress _

instance showBaseAddress :: Show BaseAddress where
  show = genericShow

newtype Credential = Credential String
derive instance eqCredential :: Eq Credential
derive instance genericCredential :: Generic Credential _
derive instance ordCredential :: Ord Credential

instance showCredential :: Show Credential where
  show = genericShow
-- Below comes from Plutus API:
-- data Credential = PubKeyCredential String | ScriptCredential String

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
