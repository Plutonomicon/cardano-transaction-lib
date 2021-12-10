module Main where

import Prelude
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map(..))

newtype Transaction = Transaction {
  body :: TxBody,
  witness_set :: TransactionWitnessSet,
  is_valid :: Boolean,
  auxiliary_data :: Maybe AuxiliaryData
}

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

newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Maybe (Array Vkeywitness),
    native_scripts :: Maybe Unit, -- NativeScripts,
    bootstraps :: Maybe Unit, -- BootstrapWitnesses,
    plutus_scripts :: Maybe (Array PlutusScript),
    plutus_data :: Maybe (Array PlutusData),
    redeemers :: Maybe (Array Redeemer)
  }

newtype NetworkId = NetworkId Int

newtype RequiredSigner = RequiredSigner String

newtype CurrencySymbol = CurrencySymbol String

newtype TokenName = TokenName String

newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt.BigInt))

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

newtype Vkey = Vkey String -- (bech32)

newtype Ed25519Signature = Ed25519Signature String -- (bech32)

newtype PlutusScript = PlutusScript String

newtype PlutusData = PlutusData String 
-- TODO - we need a capability to encode/decode Datum from/to serialized format
-- see `makeIsDataIndexed` 

newtype Redeemer = Redeemer
  { tag :: RedeemerTag, -- ScriptPurpose: 'spending' 'minting' etc
    index :: BigInt.BigInt,
    data :: PlutusData,
    ex_units :: (MemExUnits /\ CpuExUnits)
  }

newtype MemExUnits = MemExUnits BigInt.BigInt

newtype CpuExUnits = CpuExUnits BigInt.BigInt

data RedeemerTag = Spend | Mint | Cert | Reward

type AuxiliaryData = Unit -- this is big and weird in serialization-lib


newtype TransactionInput = TransactionInput
  { transaction_id :: String, -- TransactionHash
    index :: BigInt.BigInt -- u32 TransactionIndex
  }

newtype TransactionOutput = TransactionOutput-- array of,
  { address :: Address,
    amount :: Value,
    data_hash :: Maybe String -- DataHash>,
  }

newtype Coin = Coin BigInt.BigInt

newtype Slot = Slot BigInt.BigInt

newtype Address = Address 
  { "AddrType" :: BaseAddress
  }

newtype BaseAddress = BaseAddress
  { network :: Int, -- u8,
    stake :: Credential,
    payment :: Credential
  }

newtype Credential = Credential String

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
