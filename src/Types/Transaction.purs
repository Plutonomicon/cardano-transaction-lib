module Types.Transaction
  ( AuxiliaryData(..)
  , AuxiliaryDataHash(..)
  , BootstrapWitness
  , Certificate(..)
  , CostModel(..)
  , Costmdls(..)
  , DataHash(..)
  , DatumHash
  , Ed25519Signature(..)
  , Epoch(..)
  , ExUnitPrices
  , ExUnits
  , GeneralTransactionMetadata(..)
  , GenesisHash(..)
  , Language(..)
  , Mint(..)
  , NativeScript(..)
  , Nonce(..)
  , ProposedProtocolParameterUpdates(..)
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey(..)
  , Redeemer(..)
  , RequiredSigner(..)
  , ScriptDataHash(..)
  , SubCoin
  , Transaction(..)
  , TransactionHash(..)
  , TransactionInput(..)
  , TransactionMetadatum(..)
  , TransactionMetadatumLabel(..)
  , TransactionOutput(..)
  , TransactionWitnessSet(..)
  , TxBody(..)
  , TxOut
  , UnitInterval
  , Update
  , Utxo
  , UtxoM(..)
  , Vkey(..)
  , Vkeywitness(..)
  , _auxiliaryData
  , _auxiliaryDataHash
  , _body
  , _bootstraps
  , _certs
  , _collateral
  , _fee
  , _inputs
  , _isValid
  , _mint
  , _nativeScripts
  , _networkId
  , _outputs
  , _plutusData
  , _plutusScripts
  , _redeemers
  , _requiredSigners
  , _scriptDataHash
  , _ttl
  , _update
  , _validityStartInterval
  , _vkeys
  , _withdrawals
  , _witnessSet
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array (union)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import FromData (class FromData, fromData)
import Helpers ((</>), (<<>>), appendMap, appendRightMap)
import Serialization.Address (Address, NetworkId, RewardAddress, Slot(Slot))
import Serialization.Hash (Ed25519KeyHash)
import ToData (class ToData, toData)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Types.Scripts (PlutusScript)
import Types.Value (Coin, NonAdaAsset, Value)
import Types.PlutusData (PlutusData(Constr))

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

derive instance Generic Transaction _
derive instance Eq Transaction
derive instance Newtype Transaction _

instance Show Transaction where
  show = genericShow

instance Semigroup Transaction where
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

instance Monoid Transaction where
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

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = lens' \(Transaction rec@{ witness_set }) ->
  Tuple witness_set \ws -> Transaction rec { witness_set = ws }

_isValid :: Lens' Transaction Boolean
_isValid = lens' \(Transaction rec@{ is_valid }) ->
  Tuple is_valid \iv -> Transaction rec { is_valid = iv }

_auxiliaryData :: Lens' Transaction (Maybe AuxiliaryData)
_auxiliaryData = lens' \(Transaction rec@{ auxiliary_data }) ->
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

derive instance Generic TxBody _
derive instance Newtype TxBody _
derive newtype instance Eq TxBody

instance Show TxBody where
  show = genericShow

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

newtype ScriptDataHash = ScriptDataHash ByteArray

derive instance Newtype ScriptDataHash _
derive instance Generic ScriptDataHash _
derive newtype instance Eq ScriptDataHash

instance Show ScriptDataHash where
  show = genericShow

newtype Mint = Mint NonAdaAsset

derive instance Generic Mint _
derive instance Newtype Mint _
derive newtype instance Eq Mint
derive newtype instance Semigroup Mint
derive newtype instance Monoid Mint

instance Show Mint where
  show = genericShow

newtype AuxiliaryDataHash = AuxiliaryDataHash String

derive instance newtypeAuxiliaryDataHash :: Newtype AuxiliaryDataHash _
derive newtype instance eqAuxiliaryDataHash :: Eq AuxiliaryDataHash
derive instance Generic AuxiliaryDataHash _

instance Show AuxiliaryDataHash where
  show = genericShow

type Update =
  { proposed_protocol_parameter_updates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

newtype ProposedProtocolParameterUpdates = ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

derive instance Newtype ProposedProtocolParameterUpdates _

derive newtype instance Eq ProposedProtocolParameterUpdates

derive instance Generic ProposedProtocolParameterUpdates _

instance Show ProposedProtocolParameterUpdates where
  show = genericShow

newtype GenesisHash = GenesisHash String

derive instance Newtype GenesisHash _
derive newtype instance Eq GenesisHash
derive instance Generic GenesisHash _

instance Show GenesisHash where
  show = genericShow

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

derive instance Newtype Costmdls _
derive newtype instance Eq Costmdls
derive instance Generic Costmdls _

instance Show Costmdls where
  show = genericShow

data Language = PlutusV1

derive instance Eq Language
derive instance Ord Language
derive instance Generic Language _

instance Show Language where
  show = genericShow

newtype CostModel = CostModel (Array UInt)

derive instance Newtype CostModel _
derive newtype instance Eq CostModel
derive instance Generic Nonce _
derive instance Generic CostModel _

instance Show CostModel where
  show = genericShow

instance Show Nonce where
  show = genericShow

type ProtocolVersion =
  { major :: UInt
  , minor :: UInt
  }

newtype Nonce = Nonce String

derive instance Newtype Nonce _
derive newtype instance Eq Nonce

type UnitInterval =
  { numerator :: BigInt
  , denominator :: BigInt
  }

newtype Epoch = Epoch UInt

derive instance Newtype Epoch _
derive instance Generic Epoch _
derive newtype instance Eq Epoch

instance Show Epoch where
  show = genericShow

data Certificate
  = StakeRegistration
  | StakeDeregistration
  | StakeDelegation
  | PoolRegistration
  | PoolRetirement
  | GenesisKeyDelegation
  | MoveInstantaneousRewardsCert

derive instance Eq Certificate
derive instance Generic Certificate _

instance Show Certificate where
  show = genericShow

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

_auxiliaryDataHash :: Lens' TxBody (Maybe AuxiliaryDataHash)
_auxiliaryDataHash = _Newtype <<< prop (SProxy :: SProxy "auxiliary_data_hash")

_validityStartInterval :: Lens' TxBody (Maybe Slot)
_validityStartInterval =
  _Newtype <<< prop (SProxy :: SProxy "validity_start_interval")

_mint :: Lens' TxBody (Maybe Mint)
_mint = _Newtype <<< prop (SProxy :: SProxy "mint")

_scriptDataHash :: Lens' TxBody (Maybe ScriptDataHash)
_scriptDataHash = _Newtype <<< prop (SProxy :: SProxy "script_data_hash")

_collateral :: Lens' TxBody (Maybe (Array TransactionInput))
_collateral = _Newtype <<< prop (SProxy :: SProxy "collateral")

_requiredSigners :: Lens' TxBody (Maybe (Array RequiredSigner))
_requiredSigners = _Newtype <<< prop (SProxy :: SProxy "required_signers")

_networkId :: Lens' TxBody (Maybe NetworkId)
_networkId = _Newtype <<< prop (SProxy :: SProxy "network_id")

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

_nativeScripts :: Lens' TransactionWitnessSet (Maybe (Array NativeScript))
_nativeScripts = lens' \(TransactionWitnessSet rec@{ native_scripts }) ->
  Tuple native_scripts \ns -> TransactionWitnessSet rec { native_scripts = ns }

_bootstraps :: Lens' TransactionWitnessSet (Maybe (Array BootstrapWitness))
_bootstraps = lens' \(TransactionWitnessSet rec@{ bootstraps }) ->
  Tuple bootstraps \bs -> TransactionWitnessSet rec { bootstraps = bs }

_plutusScripts :: Lens' TransactionWitnessSet (Maybe (Array PlutusScript))
_plutusScripts = lens' \(TransactionWitnessSet rec@{ plutus_scripts }) ->
  Tuple plutus_scripts \ps -> TransactionWitnessSet rec { plutus_scripts = ps }

_plutusData :: Lens' TransactionWitnessSet (Maybe (Array PlutusData))
_plutusData = lens' \(TransactionWitnessSet rec@{ plutus_data }) ->
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

derive instance Newtype RequiredSigner _
derive newtype instance Eq RequiredSigner
derive newtype instance Ord RequiredSigner
derive instance Generic RequiredSigner _

instance Show RequiredSigner where
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
derive newtype instance Ord Vkey

instance Show Vkey where
  show = genericShow

newtype PublicKey = PublicKey Bech32String

derive instance Generic PublicKey _
derive instance Newtype PublicKey _
derive newtype instance Eq PublicKey
derive newtype instance Ord PublicKey

instance Show PublicKey where
  show = genericShow

newtype Ed25519Signature = Ed25519Signature Bech32String

derive instance Generic Ed25519Signature _
derive newtype instance Eq Ed25519Signature
derive newtype instance Ord Ed25519Signature

instance Show Ed25519Signature where
  show = genericShow

newtype Redeemer = Redeemer
  { tag :: RedeemerTag
  , index :: BigInt
  , data :: PlutusData
  , ex_units :: ExUnits
  }

derive instance Generic Redeemer _
derive newtype instance Eq Redeemer
derive newtype instance Ord Redeemer

instance Show Redeemer where
  show = genericShow

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , native_scripts :: Maybe (Array NativeScript)
  , plutus_scripts :: Maybe (Array PlutusScript)
  }

derive newtype instance Eq AuxiliaryData
derive instance Generic AuxiliaryData _

instance Show AuxiliaryData where
  show = genericShow

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
  GeneralTransactionMetadata (Map TransactionMetadatumLabel TransactionMetadatum)

derive instance Newtype GeneralTransactionMetadata _

derive newtype instance Eq GeneralTransactionMetadata
derive instance Generic GeneralTransactionMetadata _

instance Show GeneralTransactionMetadata where
  show = genericShow

-- This Semigroup instance simply takes the Last value for duplicate keys
-- to avoid a Semigroup instance for TransactionMetadatum.
-- Do we want to avoid a Semigroup instance for TransactionMetadatum? Recursion
-- is fine but how to combine Text with Bytes for example? One would have to take
-- precedence and replace the other.
instance Semigroup GeneralTransactionMetadata where
  append (GeneralTransactionMetadata hm) (GeneralTransactionMetadata hm') =
    GeneralTransactionMetadata $ hm `appendRightMap` hm'

instance Monoid GeneralTransactionMetadata where
  mempty = GeneralTransactionMetadata Map.empty

newtype TransactionMetadatumLabel = TransactionMetadatumLabel BigInt

derive instance Newtype TransactionMetadatumLabel _
derive newtype instance Eq TransactionMetadatumLabel
derive newtype instance Ord TransactionMetadatumLabel
derive instance Generic TransactionMetadatumLabel _

instance Show TransactionMetadatumLabel where
  show = genericShow

data TransactionMetadatum
  = MetadataMap (Map TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int
  | Bytes ByteArray
  | Text String

derive instance Eq TransactionMetadatum
derive instance Generic TransactionMetadatum _

instance Show TransactionMetadatum where
  show x = genericShow x

data NativeScript
  = ScriptPubkey Ed25519KeyHash
  | ScriptAll (Array NativeScript)
  | ScriptAny (Array NativeScript)
  | ScriptNOfK Int (Array NativeScript)
  | TimelockStart Slot
  | TimelockExpiry Slot

derive instance Eq NativeScript
derive instance Generic NativeScript _

instance Show NativeScript where
  show x = genericShow x

newtype TransactionInput = TransactionInput
  { transaction_id :: TransactionHash
  , index :: UInt
  }

derive instance Newtype TransactionInput _
derive instance Generic TransactionInput _
derive newtype instance Eq TransactionInput
derive newtype instance Ord TransactionInput

instance Show TransactionInput where
  show = genericShow

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance FromData TransactionInput where
  fromData (Constr n [ txId, idx ]) | n == zero =
    TransactionInput <$>
      ({ transaction_id: _, index: _ } <$> fromData txId <*> fromData idx)
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance ToData TransactionInput where
  toData (TransactionInput { transaction_id, index }) =
    Constr zero [ toData transaction_id, toData index ]

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , data_hash :: Maybe DataHash
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

-- `Constr` is used for indexing, and `TransactionOutput` is always zero-indexed
instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, dh ]) | n == zero =
    TransactionOutput <$>
      ( { address: _, amount: _, data_hash: _ }
          <$> fromData addr
          <*> fromData amt
          <*> fromData dh
      )
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionOutput` is always zero-indexed
instance ToData TransactionOutput where
  toData (TransactionOutput { address, amount, data_hash }) =
    Constr zero [ toData address, toData amount, toData data_hash ]

-- For convenience of Haskell code:
type TxOut = TransactionOutput

newtype UtxoM = UtxoM Utxo

derive instance Newtype UtxoM _
derive newtype instance Show UtxoM

type Utxo = Map TransactionInput TransactionOutput

-- | 32-bytes blake2b256 hash of a tx body.
-- | NOTE. Plutus docs might incorrectly state that it uses
-- |       SHA256 for this purposes.
newtype TransactionHash = TransactionHash ByteArray

derive instance Generic TransactionHash _
derive instance Newtype TransactionHash _
derive newtype instance Eq TransactionHash
derive newtype instance FromData TransactionHash
derive newtype instance Ord TransactionHash
derive newtype instance ToData TransactionHash

instance Show TransactionHash where
  show = genericShow

newtype DataHash = DataHash ByteArray

derive instance Generic DataHash _
derive instance Newtype DataHash _
derive newtype instance Eq DataHash
derive newtype instance FromData DataHash
derive newtype instance Ord DataHash
derive newtype instance ToData DataHash

instance Show DataHash where
  show = genericShow

-- To help with people copying & pasting code from Haskell to Purescript
type DatumHash = DataHash

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
