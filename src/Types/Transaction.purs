module Types.Transaction where

import Prelude

import Control.Apply (lift2)
import Data.Array (union)
import Data.BigInt (BigInt, toNumber)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap, empty)
import Data.HashMap (unionWith) as HashMap
import Data.Map (Map)
import Data.Map (unionWith) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Maybe.Last (Last(Last))
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)

import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Types.Value (Coin, Value)

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

-- Semigroup for Transaction only appends transactions that are both valid,
-- if both are false, take the rightmost.
instance semigroupTransaction :: Semigroup Transaction where
  append (Transaction tx) (Transaction tx') =
    Transaction
      if tx.is_valid then
        if tx'.is_valid then
          { body: tx.body <> tx'.body
          , witness_set: tx.witness_set <> tx'.witness_set
          , is_valid: true
          , auxiliary_data: tx.auxiliary_data <> tx'.auxiliary_data
          }
        else tx
      else if tx'.is_valid then tx'
      else tx' -- default to rightmost if both false.

instance monoidTransaction :: Monoid Transaction where
  mempty = Transaction
    { body: mempty
    , witness_set: mempty
    , is_valid: false
    , auxiliary_data: Nothing
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

derive instance newtypeTxBody :: Newtype TxBody _
derive newtype instance eqTxBody :: Eq TxBody

instance semigroupTxBody :: Semigroup TxBody where
  append (TxBody txB) (TxBody txB') = TxBody
    { inputs: txB.inputs `union` txB'.inputs
    , outputs: txB.outputs `union` txB'.outputs
    , fee: txB.fee <> txB'.fee
    -- Slots add (in Maybe context), is this correct? Do we want First or Last? Maybe Min or Max?
    , ttl: txB.ttl <> txB'.ttl
    , certs: lift2 union txB.certs txB'.certs
    , withdrawals: lift2 appendMap txB.withdrawals txB'.withdrawals
    -- Use the Last for update, do we want something more sophisicated like latest/max Epoch?
    -- I don't think we want Semigroup on Epoch and ProtocolParamUpdate do we?
    -- Should we be using these protocol parameters in our logic?
    , update: txB.update <<>> txB'.update
    -- Don't use String newtype to just append strings, presumably we don't want to just append hashes.
    , auxiliary_data_hash: txB.auxiliary_data_hash <<>> txB'.auxiliary_data_hash
    -- Slots add, same as ttl. Do we want First or Last? Maybe Min or Max.
    , validity_start_interval:
        txB.validity_start_interval <> txB'.validity_start_interval
    , mint: txB.mint <> txB'.mint
    -- Don't use String newtype to just append strings, presumably we don't want to just append hashes.
    , script_data_hash: txB.script_data_hash <<>> txB'.script_data_hash
    , collateral: lift2 union txB.collateral txB'.collateral
    , required_signers: lift2 union txB.required_signers txB'.required_signers
    -- Would we prefer that mainnet takes precedence over testnet so that one occurence of mainnet fixes it forever?
    -- That could be a bit too restrictive.
    , network_id: txB.network_id <<>> txB'.network_id
    }

instance monoidTxBody :: Monoid TxBody where
  mempty = TxBody
    { inputs: mempty
    , outputs: mempty
    , fee: mempty
    , ttl: Nothing
    , certs: Nothing
    , withdrawals: Nothing
    , update: Nothing
    , auxiliary_data_hash: Nothing
    , validity_start_interval: Nothing
    , mint: Nothing
    , script_data_hash: Nothing
    , collateral: Nothing
    , required_signers: Nothing
    , network_id: Nothing
    }

-- We could pick First but Last allows for convenient transforming later in the code.
appendLastMaybe :: forall (a :: Type). Maybe a -> Maybe a -> Maybe a
appendLastMaybe m m' = Last m <> Last m' # \(Last m'') -> m''

infixr 5 appendLastMaybe as <<>>

-- Provide an append for Maps where the value has as Semigroup instance
appendMap
  :: forall (k :: Type) (v :: Type)
   . Ord k
  => Semigroup v
  => Map k v
  -> Map k v
  -> Map k v
appendMap = Map.unionWith (<>)

newtype ScriptDataHash = ScriptDataHash String

derive instance newtypeScriptDataHash :: Newtype ScriptDataHash _
derive newtype instance eqScriptDataHash :: Eq ScriptDataHash
-- I think these are a bad idea as we don't want to just append Strings for a hash
-- do we? Maybe Rightmost (Last) would make sense?
-- derive newtype instance semigroupScriptDataHash :: Semigroup ScriptDataHash
-- derive newtype instance monoidScriptDataHash :: Monoid ScriptDataHash

newtype Mint = Mint Value

derive instance newtypeMint :: Newtype Mint _
derive newtype instance eqMint :: Eq Mint
derive newtype instance semigroupMint :: Semigroup Mint
derive newtype instance monoidMint :: Monoid Mint

newtype AuxiliaryDataHash = AuxiliaryDataHash String

derive instance newtypeAuxiliaryDataHash :: Newtype AuxiliaryDataHash _
derive newtype instance eqAuxiliaryDataHash :: Eq AuxiliaryDataHash
-- -- Similar here, do we want First or Last instead?
-- derive newtype instance semigroupAuxiliaryDataHash :: Semigroup AuxiliaryDataHash
-- derive newtype instance monoidAuxiliaryDataHash :: Monoid AuxiliaryDataHash

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

type RewardAddress =
  { network :: UInt
  , payment :: StakeCredential
  }

data StakeCredential
  = StakeCredentialKey Ed25519KeyHash
  | StakeCredentialScript ScriptHash

derive instance eqStakeCredential :: Eq StakeCredential
derive instance ordStakeCredential :: Ord StakeCredential
derive instance genericStakeCredential :: Generic StakeCredential _

instance showStakeCredential :: Show StakeCredential where
  show = genericShow

newtype Ed25519KeyHash = Ed25519KeyHash ByteArray

derive instance genericEd25519KeyHash :: Generic Ed25519KeyHash _
derive instance newtypeEd25519KeyHash :: Newtype Ed25519KeyHash _
derive newtype instance eqEd25519KeyHash :: Eq Ed25519KeyHash
derive newtype instance ordEd25519KeyHash :: Ord Ed25519KeyHash

instance showEd25519KeyHash :: Show Ed25519KeyHash where
  show = genericShow

newtype ScriptHash = ScriptHash ByteArray

derive instance genericScriptHash :: Generic ScriptHash _
derive instance newtypeScriptHash :: Newtype ScriptHash _
derive newtype instance eqScriptHash :: Eq ScriptHash
derive newtype instance ordScriptHash :: Ord ScriptHash

instance showScriptHash :: Show ScriptHash where
  show = genericShow

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

instance semigroupTransactionWitnessSet :: Semigroup TransactionWitnessSet where
  append (TransactionWitnessSet tws) (TransactionWitnessSet tws') =
    TransactionWitnessSet
      { vkeys: lift2 union tws.vkeys tws'.vkeys
      , native_scripts: lift2 union tws.native_scripts tws'.native_scripts
      , bootstraps: lift2 union tws.bootstraps tws'.bootstraps
      , plutus_scripts: lift2 union tws.plutus_scripts tws'.plutus_scripts
      , plutus_data: lift2 union tws.plutus_data tws'.plutus_data
      , redeemers: lift2 union tws.redeemers tws'.redeemers
      }

instance monoidTransactionWitnessSet :: Monoid TransactionWitnessSet where
  mempty = TransactionWitnessSet
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

data NetworkId
  = Mainnet
  | Testnet

derive instance eqNetworkId :: Eq NetworkId

newtype RequiredSigner = RequiredSigner String

derive instance newtypeRequiredSigner :: Newtype RequiredSigner _
derive newtype instance eqRequiredSigner :: Eq RequiredSigner

newtype Bech32 = Bech32 String

derive instance Generic Bech32 _
derive instance Newtype Bech32 _
derive newtype instance eqBech32 :: Eq Bech32
derive newtype instance Ord Bech32
instance Show Bech32 where
  show = genericShow

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

newtype PublicKey = PublicKey Bech32

derive instance Generic PublicKey _
derive instance Newtype PublicKey _
derive newtype instance Eq PublicKey

instance Show PublicKey where
  show = genericShow

newtype Ed25519Signature = Ed25519Signature Bech32

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

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , native_scripts :: Maybe (Array NativeScript)
  , plutus_scripts :: Maybe (Array PlutusScript)
  }

derive newtype instance eqAuxiliaryData :: Eq AuxiliaryData

instance semigroupAuxiliaryData :: Semigroup AuxiliaryData where
  append (AuxiliaryData ad) (AuxiliaryData ad') =
    AuxiliaryData
      { metadata: ad.metadata <> ad'.metadata
      , native_scripts: lift2 union ad.native_scripts ad'.native_scripts
      , plutus_scripts: lift2 union ad.plutus_scripts ad'.plutus_scripts
      }

instance monoidAuxiliaryData :: Monoid AuxiliaryData where
  mempty = AuxiliaryData
    { metadata: Nothing
    , native_scripts: Nothing
    , plutus_scripts: Nothing
    }

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata (HashMap TransactionMetadatumLabel TransactionMetadatum)

derive instance newtypeGeneralTransactionMetadata :: Newtype GeneralTransactionMetadata _

derive newtype instance eqGeneralTransactionMetadata :: Eq GeneralTransactionMetadata

-- This Semigroup instance simply takes the Last value for duplicate keys
-- to avoid a Semigroup instance for TransactionMetadatum.
-- Do we want to avoid a Semigroup instance for TransactionMetadatum? Recursion
-- is fine but how to combine Text with Bytes for example? One would have to take
-- precedence and replace the other.
instance semigroupGeneralTransactionMetadata :: Semigroup GeneralTransactionMetadata where
  append (GeneralTransactionMetadata hm) (GeneralTransactionMetadata hm') =
    GeneralTransactionMetadata $ hm `appendRightHashMap` hm'

instance monoidGeneralTransactionMetadata :: Monoid GeneralTransactionMetadata where
  mempty = GeneralTransactionMetadata empty

-- Provide an append for HashMaps where we take the rightmost value
appendRightHashMap
  :: forall (k :: Type) (v :: Type)
   . Hashable k
  => HashMap k v
  -> HashMap k v
  -> HashMap k v
appendRightHashMap = HashMap.unionWith (flip const)

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt

derive instance newtypeTransactionMetadatumLabel :: Newtype TransactionMetadatumLabel _

derive newtype instance eqTransactionMetadatumLabel :: Eq TransactionMetadatumLabel

-- Hashable requires a = b implies hash a = hash b so I think losing
instance hashableTransactionMetadatumLabel :: Hashable TransactionMetadatumLabel where
  hash (TransactionMetadatumLabel bi) = hash $ toNumber bi

data TransactionMetadatum
  = MetadataMap (HashMap TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int
  | Bytes ByteArray
  | Text String

derive instance eqTransactionMetadatum :: Eq TransactionMetadatum

data NativeScript
  = ScriptPubkey
  | ScriptAll
  | ScriptAny
  | ScriptNOfK
  | TimelockStart
  | TimelockExpiry

derive instance eqNativeScript :: Eq NativeScript
derive instance Generic NativeScript _

instance Show NativeScript where
  show = genericShow

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

newtype Slot = Slot BigInt

derive instance newtypeSlot :: Newtype Slot _
derive newtype instance eqSlot :: Eq Slot

-- Is this really what we want?
instance semigroupSlot :: Semigroup Slot where
  append (Slot s1) (Slot s2) = Slot $ s1 + s2

instance monoidSlot :: Monoid Slot where
  mempty = Slot zero

newtype Address = Address
  { "AddrType" :: BaseAddress
  }

derive instance genericAddress :: Generic Address _
derive instance newtypeAddress :: Newtype Address _
derive newtype instance eqAddress :: Eq Address
derive newtype instance ordAddress :: Ord Address

instance showAddress :: Show Address where
  show = genericShow

newtype BaseAddress = BaseAddress
  { network :: UInt -- UInt8
  , stake :: StakeCredential
  , payment :: PaymentCredential
  }

derive instance genericBaseAddress :: Generic BaseAddress _
derive instance newtypeBaseAddress :: Newtype BaseAddress _
derive newtype instance eqBaseAddress :: Eq BaseAddress
derive newtype instance ordBaseAddress :: Ord BaseAddress

instance showBaseAddress :: Show BaseAddress where
  show = genericShow

data PaymentCredential
  = PaymentCredentialKey Ed25519KeyHash
  | PaymentCredentialScript ScriptHash

derive instance genericPaymentCredential :: Generic PaymentCredential _
derive instance eqPaymentCredential :: Eq PaymentCredential
derive instance ordPaymentCredential :: Ord PaymentCredential

instance showPaymentCredential :: Show PaymentCredential where
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
