module Types.Transaction where

import Prelude

import Control.Apply (lift2)
import Data.Array (union)
import Data.BigInt (BigInt, toNumber)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap, empty)
import Data.Lens (lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Helpers ((</>), (<<>>), appendMap, appendRightHashMap)
import Serialization.Address (Address, NetworkId, RewardAddress, Slot(Slot))
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Types.Scripts (PlutusScript)
import Types.Value (Coin, Value)
import Serialization.Hash (Ed25519KeyHash)

--------------------------------------------------------------------------------
-- `Transaction`
--------------------------------------------------------------------------------
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

instance semigroupTransaction :: Semigroup Transaction where
  append (Transaction tx) (Transaction tx') =
    Transaction
      { body: txCheck tx.body <> txCheck' tx'.body
      , witness_set: txCheck tx.witness_set <> txCheck' tx'.witness_set
      , is_valid: tx.is_valid && tx'.is_valid
      , auxiliary_data: txCheck tx.auxiliary_data <> txCheck' tx'.auxiliary_data
      }
    where
    txCheck :: forall (m :: Type). Monoid m => m -> m
    txCheck = guard tx.is_valid

    txCheck' :: forall (m :: Type). Monoid m => m -> m
    txCheck' = guard tx'.is_valid

instance monoidTransaction :: Monoid Transaction where
  mempty = Transaction
    { body: mempty
    , witness_set: mempty
    , is_valid: true
    , auxiliary_data: Nothing
    }

--------------------------------------------------------------------------------
-- `Transaction` Lenses
--------------------------------------------------------------------------------
_body :: Lens' Transaction TxBody
_body = lens' \(Transaction rec@{ body }) ->
  Tuple body \bod -> Transaction rec { body = bod }

_witness_set :: Lens' Transaction TransactionWitnessSet
_witness_set = lens' \(Transaction rec@{ witness_set }) ->
  Tuple witness_set \ws -> Transaction rec { witness_set = ws }

_is_valid :: Lens' Transaction Boolean
_is_valid = lens' \(Transaction rec@{ is_valid }) ->
  Tuple is_valid \iv -> Transaction rec { is_valid = iv }

_auxiliary_data :: Lens' Transaction (Maybe AuxiliaryData)
_auxiliary_data = lens' \(Transaction rec@{ auxiliary_data }) ->
  Tuple auxiliary_data \ad -> Transaction rec { auxiliary_data = ad }

--------------------------------------------------------------------------------
-- `TxBody`
--------------------------------------------------------------------------------
-- According to https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl
-- required_signers is an Array over `VKey`s essentially. But some comments at
-- the bottom say it's Maybe?
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
    , ttl: lift2 lowerbound txB.ttl txB'.ttl
    , certs: lift2 union txB.certs txB'.certs
    , withdrawals: lift2 appendMap txB.withdrawals txB'.withdrawals
    , update: txB.update </> txB'.update
    , auxiliary_data_hash: txB.auxiliary_data_hash </> txB'.auxiliary_data_hash
    , validity_start_interval:
        lift2 lowerbound
          txB.validity_start_interval
          txB'.validity_start_interval
    , mint: txB.mint <> txB'.mint
    , script_data_hash: txB.script_data_hash </> txB'.script_data_hash
    , collateral: lift2 union txB.collateral txB'.collateral
    , required_signers: lift2 union txB.required_signers txB'.required_signers
    , network_id: txB.network_id </> txB'.network_id
    }
    where
    lowerbound :: Slot -> Slot -> Slot
    lowerbound (Slot x) (Slot y) = Slot $ min x y

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

newtype ScriptDataHash = ScriptDataHash String

derive instance newtypeScriptDataHash :: Newtype ScriptDataHash _
derive newtype instance eqScriptDataHash :: Eq ScriptDataHash

newtype Mint = Mint Value

derive instance newtypeMint :: Newtype Mint _
derive newtype instance eqMint :: Eq Mint
derive newtype instance semigroupMint :: Semigroup Mint
derive newtype instance monoidMint :: Monoid Mint

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

--------------------------------------------------------------------------------
-- `TxBody` Lenses
--------------------------------------------------------------------------------
_inputs :: Lens' TxBody (Array TransactionInput)
_inputs = _Newtype <<< prop (SProxy :: SProxy "inputs")

_outputs :: Lens' TxBody (Array TransactionOutput)
_outputs = _Newtype <<< prop (SProxy :: SProxy "outputs")

_fee :: Lens' TxBody (Coin)
_fee = _Newtype <<< prop (SProxy :: SProxy "fee")

_ttl :: Lens' TxBody (Maybe Slot)
_ttl = _Newtype <<< prop (SProxy :: SProxy "ttl")

_certs :: Lens' TxBody (Maybe (Array Certificate))
_certs = _Newtype <<< prop (SProxy :: SProxy "certs")

_withdrawals :: Lens' TxBody (Maybe (Map RewardAddress Coin))
_withdrawals = _Newtype <<< prop (SProxy :: SProxy "withdrawals")

_update :: Lens' TxBody (Maybe Update)
_update = _Newtype <<< prop (SProxy :: SProxy "update")

_auxiliary_data_hash :: Lens' TxBody (Maybe AuxiliaryDataHash)
_auxiliary_data_hash = _Newtype <<< prop (SProxy :: SProxy "auxiliary_data_hash")

_validity_start_interval :: Lens' TxBody (Maybe Slot)
_validity_start_interval =
  _Newtype <<< prop (SProxy :: SProxy "validity_start_interval")

_mint :: Lens' TxBody (Maybe Mint)
_mint = _Newtype <<< prop (SProxy :: SProxy "mint")

_script_data_hash :: Lens' TxBody (Maybe ScriptDataHash)
_script_data_hash = _Newtype <<< prop (SProxy :: SProxy "script_data_hash")

_collateral :: Lens' TxBody (Maybe (Array TransactionInput))
_collateral = _Newtype <<< prop (SProxy :: SProxy "collateral")

_required_signers :: Lens' TxBody (Maybe (Array RequiredSigner))
_required_signers = _Newtype <<< prop (SProxy :: SProxy "required_signers")

_network_id :: Lens' TxBody (Maybe NetworkId)
_network_id = _Newtype <<< prop (SProxy :: SProxy "network_id")

--------------------------------------------------------------------------------
-- `TransactionWitnessSet`
--------------------------------------------------------------------------------
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
      { vkeys: tws.vkeys <<>> tws'.vkeys
      , native_scripts: tws.native_scripts <<>> tws'.native_scripts
      , bootstraps: tws.bootstraps <<>> tws'.bootstraps
      , plutus_scripts: tws.plutus_scripts <<>> tws'.plutus_scripts
      , plutus_data: tws.plutus_data <<>> tws'.plutus_data
      , redeemers: tws.redeemers <<>> tws'.redeemers
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

--------------------------------------------------------------------------------
-- `TransactionWitnessSet` Lenses
--------------------------------------------------------------------------------
_vkeys :: Lens' TransactionWitnessSet (Maybe (Array Vkeywitness))
_vkeys = lens' \(TransactionWitnessSet rec@{ vkeys }) ->
  Tuple vkeys \vk -> TransactionWitnessSet rec { vkeys = vk }

_native_scripts :: Lens' TransactionWitnessSet (Maybe (Array NativeScript))
_native_scripts = lens' \(TransactionWitnessSet rec@{ native_scripts }) ->
  Tuple native_scripts \ns -> TransactionWitnessSet rec { native_scripts = ns }

_bootstraps :: Lens' TransactionWitnessSet (Maybe (Array BootstrapWitness))
_bootstraps = lens' \(TransactionWitnessSet rec@{ bootstraps }) ->
  Tuple bootstraps \bs -> TransactionWitnessSet rec { bootstraps = bs }

_plutus_scripts :: Lens' TransactionWitnessSet (Maybe (Array PlutusScript))
_plutus_scripts = lens' \(TransactionWitnessSet rec@{ plutus_scripts }) ->
  Tuple plutus_scripts \ps -> TransactionWitnessSet rec { plutus_scripts = ps }

_plutus_data :: Lens' TransactionWitnessSet (Maybe (Array PlutusData))
_plutus_data = lens' \(TransactionWitnessSet rec@{ plutus_data }) ->
  Tuple plutus_data \pd -> TransactionWitnessSet rec { plutus_data = pd }

_redeemers :: Lens' TransactionWitnessSet (Maybe (Array Redeemer))
_redeemers = lens' \(TransactionWitnessSet rec@{ redeemers }) ->
  Tuple redeemers \red -> TransactionWitnessSet rec { redeemers = red }

--------------------------------------------------------------------------------
-- Other Datatypes
--------------------------------------------------------------------------------
type BootstrapWitness =
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chain_code :: ByteArray
  , attributes :: ByteArray
  }

newtype RequiredSigner = RequiredSigner Vkey

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

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt

derive instance newtypeTransactionMetadatumLabel :: Newtype TransactionMetadatumLabel _

derive newtype instance eqTransactionMetadatumLabel :: Eq TransactionMetadatumLabel

-- Hashable requires a = b implies hash a = hash b so I think losing precision might be okay?
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
