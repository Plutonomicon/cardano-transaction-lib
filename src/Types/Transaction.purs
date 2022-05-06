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
  , GenesisDelegateHash(..)
  , GenesisHash(..)
  , Ipv4(..)
  , Ipv6(..)
  , Language(..)
  , Mint(..)
  , MIRToStakeCredentials(..)
  , MoveInstantaneousReward(..)
  , NativeScript(..)
  , Nonce(..)
  , PoolMetadata(..)
  , PoolMetadataHash(..)
  , ProposedProtocolParameterUpdates(..)
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey(..)
  , Redeemer(..)
  , Relay(..)
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
  , URL(..)
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
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import FromData (class FromData, fromData)
import Helpers ((</>), (<<>>), appendMap, appendRightMap)
import Plutus.FromPlutusType (fromPlutusType)
import Plutus.ToPlutusType (toPlutusType)
import Plutus.Types.Value (Value) as Plutus
import Serialization.Address
  ( Address
  , NetworkId
  , RewardAddress
  , Slot(Slot)
  , StakeCredential
  )
import Serialization.Hash (Ed25519KeyHash)
import Serialization.Types (BigNum, VRFKeyHash)
import ToData (class ToData, toData)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray, byteArrayToHex)
import Types.Int as Int
import Types.PlutusData (PlutusData(Constr))
import Types.RedeemerTag (RedeemerTag)
import Types.Scripts (PlutusScript)
import Cardano.Types.Value (Coin, NonAdaAsset, Value)

--------------------------------------------------------------------------------
-- `Transaction`
--------------------------------------------------------------------------------
-- note: these types are derived from the cardano-serialization-lib Sundae fork
-- the source of truth for these types should be that library and the
-- corresponding Rust types
newtype Transaction = Transaction
  { body :: TxBody
  , witnessSet :: TransactionWitnessSet
  , isValid :: Boolean
  , auxiliaryData :: Maybe AuxiliaryData
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
      , witnessSet: txCheck tx.witnessSet <> txCheck' tx'.witnessSet
      , isValid: tx.isValid && tx'.isValid
      , auxiliaryData: txCheck tx.auxiliaryData <> txCheck' tx'.auxiliaryData
      }
    where
    txCheck :: forall (m :: Type). Monoid m => m -> m
    txCheck = guard tx.isValid

    txCheck' :: forall (m :: Type). Monoid m => m -> m
    txCheck' = guard tx'.isValid

instance Monoid Transaction where
  mempty = Transaction
    { body: mempty
    , witnessSet: mempty
    , isValid: true
    , auxiliaryData: Nothing
    }

--------------------------------------------------------------------------------
-- `Transaction` Lenses
--------------------------------------------------------------------------------
_body :: Lens' Transaction TxBody
_body = lens' \(Transaction rec@{ body }) ->
  Tuple body \bod -> Transaction rec { body = bod }

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = lens' \(Transaction rec@{ witnessSet }) ->
  Tuple witnessSet \ws -> Transaction rec { witnessSet = ws }

_isValid :: Lens' Transaction Boolean
_isValid = lens' \(Transaction rec@{ isValid }) ->
  Tuple isValid \iv -> Transaction rec { isValid = iv }

_auxiliaryData :: Lens' Transaction (Maybe AuxiliaryData)
_auxiliaryData = lens' \(Transaction rec@{ auxiliaryData }) ->
  Tuple auxiliaryData \ad -> Transaction rec { auxiliaryData = ad }

--------------------------------------------------------------------------------
-- `TxBody`
--------------------------------------------------------------------------------
-- According to https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl
-- requiredSigners is an Array over `VKey`s essentially. But some comments at
-- the bottom say it's Maybe?
newtype TxBody = TxBody
  { inputs :: Array TransactionInput
  , outputs :: Array TransactionOutput
  , fee :: Coin
  , ttl :: Maybe Slot
  , certs :: Maybe (Array Certificate)
  , withdrawals :: Maybe (Map RewardAddress Coin)
  , update :: Maybe Update
  , auxiliaryDataHash :: Maybe AuxiliaryDataHash
  , validityStartInterval :: Maybe Slot
  , mint :: Maybe Mint
  , scriptDataHash :: Maybe ScriptDataHash
  , collateral :: Maybe (Array TransactionInput)
  , requiredSigners :: Maybe (Array RequiredSigner)
  , networkId :: Maybe NetworkId
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
    , auxiliaryDataHash: txB.auxiliaryDataHash </> txB'.auxiliaryDataHash
    , validityStartInterval:
        lift2 lowerbound
          txB.validityStartInterval
          txB'.validityStartInterval
    , mint: txB.mint <> txB'.mint
    , scriptDataHash: txB.scriptDataHash </> txB'.scriptDataHash
    , collateral: lift2 union txB.collateral txB'.collateral
    , requiredSigners: lift2 union txB.requiredSigners txB'.requiredSigners
    , networkId: txB.networkId </> txB'.networkId
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
    , auxiliaryDataHash: Nothing
    , validityStartInterval: Nothing
    , mint: Nothing
    , scriptDataHash: Nothing
    , collateral: Nothing
    , requiredSigners: Nothing
    , networkId: Nothing
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

newtype AuxiliaryDataHash = AuxiliaryDataHash ByteArray

derive instance Generic AuxiliaryDataHash _
derive instance Newtype AuxiliaryDataHash _
derive newtype instance Eq AuxiliaryDataHash

instance Show AuxiliaryDataHash where
  show = genericShow

type Update =
  { proposedProtocolParameterUpdates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

newtype ProposedProtocolParameterUpdates =
  ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

derive instance Newtype ProposedProtocolParameterUpdates _

derive newtype instance Eq ProposedProtocolParameterUpdates

derive instance Generic ProposedProtocolParameterUpdates _

instance Show ProposedProtocolParameterUpdates where
  show = genericShow

newtype GenesisHash = GenesisHash ByteArray

derive instance Newtype GenesisHash _
derive newtype instance Eq GenesisHash
derive newtype instance Ord GenesisHash
derive instance Generic GenesisHash _

instance Show GenesisHash where
  show = genericShow

type ProtocolParamUpdate =
  { minfeeA :: Maybe Coin
  , minfeeB :: Maybe Coin
  , maxBlockBodySize :: Maybe UInt
  , maxTxSize :: Maybe UInt
  , maxBlockHeaderSize :: Maybe UInt
  , keyDeposit :: Maybe Coin
  , poolDeposit :: Maybe Coin
  , maxEpoch :: Maybe Epoch
  , nOpt :: Maybe UInt
  , poolPledgeInfluence :: Maybe UnitInterval
  , expansionRate :: Maybe UnitInterval
  , treasuryGrowthRate :: Maybe UnitInterval
  , d :: Maybe UnitInterval
  , extraEntropy :: Maybe Nonce
  , protocolVersion :: Maybe (Array ProtocolVersion)
  , minPoolCost :: Maybe BigNum
  , adaPerUtxoByte :: Maybe BigNum
  , costModels :: Maybe Costmdls
  , executionCosts :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe UInt
  }

type ExUnitPrices =
  { memPrice :: SubCoin
  , stepPrice :: SubCoin
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

type ProtocolVersion =
  { major :: UInt
  , minor :: UInt
  }

-- Following CSL Nonce is either None or a 32 byte hash
data Nonce = IdentityNonce | HashNonce ByteArray

derive instance Eq Nonce

instance Show Nonce where
  show = genericShow

type UnitInterval =
  { numerator :: BigNum
  , denominator :: BigNum
  }

newtype Epoch = Epoch UInt

derive instance Newtype Epoch _
derive instance Generic Epoch _
derive newtype instance Eq Epoch

instance Show Epoch where
  show = genericShow

newtype Ipv4 = Ipv4 ByteArray

derive instance Eq Ipv4
derive instance Generic Ipv4 _
derive instance Newtype Ipv4 _

instance Show Ipv4 where
  show = genericShow

newtype Ipv6 = Ipv6 ByteArray

derive instance Eq Ipv6
derive instance Generic Ipv6 _
derive instance Newtype Ipv6 _

instance Show Ipv6 where
  show = genericShow

data Relay
  = SingleHostAddr
      { port :: Maybe Int
      , ipv4 :: Maybe Ipv4
      , ipv6 :: Maybe Ipv6
      }
  | SingleHostName
      { port :: Maybe Int
      , dnsName :: String
      }
  | MultiHostName { dnsName :: String }

derive instance Eq Relay
derive instance Generic Relay _

instance Show Relay where
  show = genericShow

newtype URL = URL String

derive instance Eq URL
derive instance Generic URL _
derive instance Newtype URL _

instance Show URL where
  show = genericShow

newtype PoolMetadataHash = PoolMetadataHash ByteArray

derive instance Eq PoolMetadataHash
derive instance Generic PoolMetadataHash _
derive instance Newtype PoolMetadataHash _

instance Show PoolMetadataHash where
  show = genericShow

newtype PoolMetadata = PoolMetadata
  { url :: URL
  , hash :: PoolMetadataHash
  }

derive instance Eq PoolMetadata
derive instance Generic PoolMetadata _

instance Show PoolMetadata where
  show = genericShow

newtype GenesisDelegateHash = GenesisDelegateHash ByteArray

derive instance Eq GenesisDelegateHash
derive instance Generic GenesisDelegateHash _

instance Show GenesisDelegateHash where
  show = genericShow

newtype MIRToStakeCredentials = MIRToStakeCredentials
  (Map StakeCredential Int.Int)

derive instance Eq MIRToStakeCredentials
derive instance Generic MIRToStakeCredentials _

instance Show MIRToStakeCredentials where
  show = genericShow

data MoveInstantaneousReward
  = ToOtherPot
      { pot :: Number
      , amount :: BigNum
      }
  | ToStakeCreds
      { pot :: Number
      , amounts :: MIRToStakeCredentials
      }

derive instance Eq MoveInstantaneousReward
derive instance Generic MoveInstantaneousReward _

instance Show MoveInstantaneousReward where
  show = genericShow

data Certificate
  = StakeRegistration StakeCredential
  | StakeDeregistration StakeCredential
  | StakeDelegation StakeCredential Ed25519KeyHash
  | PoolRegistration
      { operator :: Ed25519KeyHash
      , vrfKeyhash :: VRFKeyHash
      , pledge :: BigNum
      , cost :: BigNum
      , margin :: UnitInterval
      , reward_account :: RewardAddress
      , poolOwners :: Array Ed25519KeyHash
      , relays :: Array Relay
      , poolMetadata :: Maybe PoolMetadata
      }
  | PoolRetirement
      { poolKeyhash :: Ed25519KeyHash
      , epoch :: Epoch
      }
  | GenesisKeyDelegation
      { genesisHash :: GenesisHash
      , genesisDelegateHash :: GenesisDelegateHash
      , vrfKeyhash :: VRFKeyHash
      }
  | MoveInstantaneousRewardsCert MoveInstantaneousReward

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
_auxiliaryDataHash = _Newtype <<< prop (SProxy :: SProxy "auxiliaryDataHash")

_validityStartInterval :: Lens' TxBody (Maybe Slot)
_validityStartInterval =
  _Newtype <<< prop (SProxy :: SProxy "validityStartInterval")

_mint :: Lens' TxBody (Maybe Mint)
_mint = _Newtype <<< prop (SProxy :: SProxy "mint")

_scriptDataHash :: Lens' TxBody (Maybe ScriptDataHash)
_scriptDataHash = _Newtype <<< prop (SProxy :: SProxy "scriptDataHash")

_collateral :: Lens' TxBody (Maybe (Array TransactionInput))
_collateral = _Newtype <<< prop (SProxy :: SProxy "collateral")

_requiredSigners :: Lens' TxBody (Maybe (Array RequiredSigner))
_requiredSigners = _Newtype <<< prop (SProxy :: SProxy "requiredSigners")

_networkId :: Lens' TxBody (Maybe NetworkId)
_networkId = _Newtype <<< prop (SProxy :: SProxy "networkId")

--------------------------------------------------------------------------------
-- `TransactionWitnessSet`
--------------------------------------------------------------------------------
newtype TransactionWitnessSet = TransactionWitnessSet
  { vkeys :: Maybe (Array Vkeywitness)
  , nativeScripts :: Maybe (Array NativeScript)
  , bootstraps :: Maybe (Array BootstrapWitness)
  , plutusScripts :: Maybe (Array PlutusScript)
  , plutusData :: Maybe (Array PlutusData)
  , redeemers :: Maybe (Array Redeemer)
  }

derive instance Generic TransactionWitnessSet _
derive instance Newtype TransactionWitnessSet _
derive newtype instance Eq TransactionWitnessSet

instance Show TransactionWitnessSet where
  show = genericShow

instance Semigroup TransactionWitnessSet where
  append (TransactionWitnessSet tws) (TransactionWitnessSet tws') =
    TransactionWitnessSet
      { vkeys: tws.vkeys <<>> tws'.vkeys
      , nativeScripts: tws.nativeScripts <<>> tws'.nativeScripts
      , bootstraps: tws.bootstraps <<>> tws'.bootstraps
      , plutusScripts: tws.plutusScripts <<>> tws'.plutusScripts
      , plutusData: tws.plutusData <<>> tws'.plutusData
      , redeemers: tws.redeemers <<>> tws'.redeemers
      }

instance Monoid TransactionWitnessSet where
  mempty = TransactionWitnessSet
    { vkeys: Nothing
    , nativeScripts: Nothing
    , bootstraps: Nothing
    , plutusScripts: Nothing
    , plutusData: Nothing
    , redeemers: Nothing
    }

--------------------------------------------------------------------------------
-- `TransactionWitnessSet` Lenses
--------------------------------------------------------------------------------
_vkeys :: Lens' TransactionWitnessSet (Maybe (Array Vkeywitness))
_vkeys = lens' \(TransactionWitnessSet rec@{ vkeys }) ->
  Tuple vkeys \vk -> TransactionWitnessSet rec { vkeys = vk }

_nativeScripts :: Lens' TransactionWitnessSet (Maybe (Array NativeScript))
_nativeScripts = lens' \(TransactionWitnessSet rec@{ nativeScripts }) ->
  Tuple nativeScripts \ns -> TransactionWitnessSet rec { nativeScripts = ns }

_bootstraps :: Lens' TransactionWitnessSet (Maybe (Array BootstrapWitness))
_bootstraps = lens' \(TransactionWitnessSet rec@{ bootstraps }) ->
  Tuple bootstraps \bs -> TransactionWitnessSet rec { bootstraps = bs }

_plutusScripts :: Lens' TransactionWitnessSet (Maybe (Array PlutusScript))
_plutusScripts = lens' \(TransactionWitnessSet rec@{ plutusScripts }) ->
  Tuple plutusScripts \ps -> TransactionWitnessSet rec { plutusScripts = ps }

_plutusData :: Lens' TransactionWitnessSet (Maybe (Array PlutusData))
_plutusData = lens' \(TransactionWitnessSet rec@{ plutusData }) ->
  Tuple plutusData \pd -> TransactionWitnessSet rec { plutusData = pd }

_redeemers :: Lens' TransactionWitnessSet (Maybe (Array Redeemer))
_redeemers = lens' \(TransactionWitnessSet rec@{ redeemers }) ->
  Tuple redeemers \red -> TransactionWitnessSet rec { redeemers = red }

--------------------------------------------------------------------------------
-- Other Datatypes
--------------------------------------------------------------------------------
type BootstrapWitness =
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chainCode :: ByteArray
  , attributes :: ByteArray
  }

newtype RequiredSigner = RequiredSigner Ed25519KeyHash

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
  , exUnits :: ExUnits
  }

derive instance Generic Redeemer _
derive newtype instance Eq Redeemer
derive newtype instance Ord Redeemer

instance Show Redeemer where
  show = genericShow

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , nativeScripts :: Maybe (Array NativeScript)
  , plutusScripts :: Maybe (Array PlutusScript)
  }

derive newtype instance Eq AuxiliaryData
derive instance Generic AuxiliaryData _

instance Show AuxiliaryData where
  show = genericShow

instance Semigroup AuxiliaryData where
  append (AuxiliaryData ad) (AuxiliaryData ad') =
    AuxiliaryData
      { metadata: ad.metadata <> ad'.metadata
      , nativeScripts: lift2 union ad.nativeScripts ad'.nativeScripts
      , plutusScripts: lift2 union ad.plutusScripts ad'.plutusScripts
      }

instance Monoid AuxiliaryData where
  mempty = AuxiliaryData
    { metadata: Nothing
    , nativeScripts: Nothing
    , plutusScripts: Nothing
    }

newtype GeneralTransactionMetadata =
  GeneralTransactionMetadata
    (Map TransactionMetadatumLabel TransactionMetadatum)

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
  | Int Int.Int
  | Bytes ByteArray
  | Text String

derive instance Eq TransactionMetadatum
derive instance Ord TransactionMetadatum
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
  { transactionId :: TransactionHash
  , index :: UInt
  }

derive instance Newtype TransactionInput _
derive instance Generic TransactionInput _
derive newtype instance Eq TransactionInput

-- Potential fix me: the below is based on a small sample of smart contract
-- transactions, so fix this as required.
-- Not newtype derived this because it is not lexicographical as `index` is tested
-- before `transactionId`. We require lexicographical order over hexstring
-- `TransactionHash`, then `index`, seemingly inline with Cardano/Plutus.
instance Ord TransactionInput where
  compare (TransactionInput txInput) (TransactionInput txInput') =
    case compare txInput.transactionId txInput'.transactionId of
      EQ -> compare txInput.index txInput'.index
      x -> x

instance Show TransactionInput where
  show = genericShow

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance FromData TransactionInput where
  fromData (Constr n [ txId, idx ]) | n == zero =
    TransactionInput <$>
      ({ transactionId: _, index: _ } <$> fromData txId <*> fromData idx)
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance ToData TransactionInput where
  toData (TransactionInput { transactionId, index }) =
    Constr zero [ toData transactionId, toData index ]

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , dataHash :: Maybe DataHash
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
      ( { address: _, amount: _, dataHash: _ }
          <$> fromData addr
          <*> amount
          <*> fromData dh
      )
    where
    amount :: Maybe Value
    amount = unwrap <<< fromPlutusType <$>
      (fromData :: PlutusData -> Maybe Plutus.Value) amt
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionOutput` is always zero-indexed
instance ToData TransactionOutput where
  toData (TransactionOutput { address, amount, dataHash }) =
    Constr zero [ toData address, valueData, toData dataHash ]
    where
    valueData :: PlutusData
    valueData = toData $ unwrap $ toPlutusType amount

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

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord TransactionHash where
  compare (TransactionHash h) (TransactionHash h') =
    compare (byteArrayToHex h) (byteArrayToHex h')

instance Show TransactionHash where
  show = genericShow

-- Plutus actually has this as a zero indexed record
instance FromData TransactionHash where
  fromData (Constr n [ bytes ]) | n == zero = TransactionHash <$> fromData bytes
  fromData _ = Nothing

-- Plutus actually has this as a zero indexed record
instance ToData TransactionHash where
  toData (TransactionHash bytes) = Constr zero [ toData bytes ]

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
