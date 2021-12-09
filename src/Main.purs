module Main where

import Prelude
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))

newtype PreparedTx = PreparedTx
  { type :: String, -- "Tx AlonzoEra",
    description :: String, -- "",
    cborHex :: String -- bytesToHex(newTx.to_bytes())
  } 
-- newtype Transaction {
    -- body: TransactionBody,
    -- witness_set: TransactionWitnessSet,
    -- is_valid: bool,
    -- auxiliary_data: Option<AuxiliaryData>,
-- }
-- question: CBORHex also needs auxiliary data fields which do not seem to exist in this type yet.
newtype TxBody = TxBody
  { inputs :: Array TransactionInput,
    outputs :: Array TransactionOutput,
    fee :: Coin,
    ttl:: Maybe Slot,
    certs:: Maybe Unit, -- Certificates,
    withdrawals:: Maybe Unit, -- Withdrawals,
    update:: Maybe Unit, -- Update,
    auxiliary_data_hash:: Maybe String, -- AuxiliaryDataHash, - script hashes
    validity_start_interval:: Maybe Slot,
    mint:: Maybe Mint,
    script_data_hash:: Maybe String, -- ScriptDataHash,
    collateral:: Maybe TransactionInputs,
    required_signers:: Maybe RequiredSigners,
    network_id:: Maybe NetworkId
}

newtype TransactionInput = TransactionInput
  { transaction_id:: String, -- TransactionHash
    index:: BigInt.BigInt -- u32 TransactionIndex
  }

newtype TransactionOutput = TransactionOutput-- array of,
  { address:: Address,
    amount:: Value,
    data_hash:: Maybe String -- DataHash>,
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
