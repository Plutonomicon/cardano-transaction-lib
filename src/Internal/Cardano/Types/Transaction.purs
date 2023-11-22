module Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash(AuxiliaryDataHash)
  , BootstrapWitness
  , Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , GenesisKeyDelegation
      , MoveInstantaneousRewardsCert
      )
  , CostModel(CostModel)
  , Costmdls(Costmdls)
  , Ed25519Signature
  , mkEd25519Signature
  , mkFromCslEd25519Signature
  , convertEd25519Signature
  , Epoch(Epoch)
  , ExUnitPrices
  , ExUnits
  , GenesisDelegateHash(GenesisDelegateHash)
  , GenesisHash(GenesisHash)
  , Ipv4(Ipv4)
  , Ipv6(Ipv6)
  , MIRToStakeCredentials(MIRToStakeCredentials)
  , Mint(Mint)
  , MoveInstantaneousReward(ToOtherPot, ToStakeCreds)
  , Nonce(IdentityNonce, HashNonce)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , PoolPubKeyHash(PoolPubKeyHash)
  , mkPoolPubKeyHash
  , poolPubKeyHashToBech32
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey
  , mkPublicKey
  , mkFromCslPubKey
  , convertPubKey
  , Redeemer(Redeemer)
  , Relay(SingleHostAddr, SingleHostName, MultiHostName)
  , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , SubCoin
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , PoolRegistrationParams
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , URL(URL)
  , UnitInterval
  , Update
  , UtxoMap
  , pprintUtxoMap
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , _auxiliaryData
  , _auxiliaryDataHash
  , _body
  , _bootstraps
  , _certs
  , _collateral
  , _collateralReturn
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
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _totalCollateral
  , _ttl
  , _update
  , _validityStartInterval
  , _vkeys
  , _withdrawals
  , _witnessSet
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , finiteNumber
  )
import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Value (Coin, NonAdaAsset, Value, pprintValue)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.Keys
  ( ed25519SignatureFromBech32
  , publicKeyFromBech32
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.Helpers (appendMap, encodeMap, encodeTagged', (</>), (<<>>))
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
  , Slot(Slot)
  , StakeCredential
  , addressBech32
  )
import Ctl.Internal.Serialization.Hash
  ( Ed25519KeyHash
  , ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  )
import Ctl.Internal.Serialization.Keys
  ( bech32FromEd25519Signature
  , bech32FromPublicKey
  , bytesFromPublicKey
  )
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Types (Ed25519Signature, PublicKey) as Serialization
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PlutusData (PlutusData, pprintPlutusData)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash, PubKeyHash(PubKeyHash))
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag)
import Ctl.Internal.Types.RewardAddress (RewardAddress)
import Ctl.Internal.Types.Scripts (Language, PlutusScript)
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput))
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash)
import Data.Array (union)
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (union) as Set
import Data.Show.Generic (genericShow)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))

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
derive newtype instance EncodeAeson Transaction

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
-- According to https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl
-- requiredSigners is an Array over `VKey`s essentially. But some comments at
-- the bottom say it's Maybe?
newtype TxBody = TxBody
  { inputs :: Set TransactionInput
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
  , collateralReturn :: Maybe TransactionOutput
  , totalCollateral :: Maybe Coin
  , referenceInputs :: Set TransactionInput
  }

derive instance Generic TxBody _
derive instance Newtype TxBody _
derive newtype instance Eq TxBody

instance Show TxBody where
  show = genericShow

instance Semigroup TxBody where
  append (TxBody txB) (TxBody txB') = TxBody
    { inputs: txB.inputs `Set.union` txB'.inputs
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
    , referenceInputs: txB.referenceInputs <> txB'.referenceInputs
    , scriptDataHash: txB.scriptDataHash </> txB'.scriptDataHash
    , collateral: lift2 union txB.collateral txB'.collateral
    , requiredSigners: lift2 union txB.requiredSigners txB'.requiredSigners
    , networkId: txB.networkId </> txB'.networkId
    , collateralReturn: txB.collateralReturn <|> txB.collateralReturn
    , totalCollateral: txB.totalCollateral <|> txB.totalCollateral
    }
    where
    lowerbound :: Slot -> Slot -> Slot
    lowerbound (Slot x) (Slot y) = Slot $ min x y

instance Monoid TxBody where
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
    , collateralReturn: Nothing
    , totalCollateral: Nothing
    , referenceInputs: mempty
    }

instance EncodeAeson TxBody where
  encodeAeson (TxBody r) = encodeAeson $ r
    { withdrawals = encodeMap <$> r.withdrawals }

newtype ScriptDataHash = ScriptDataHash ByteArray

derive instance Newtype ScriptDataHash _
derive instance Generic ScriptDataHash _
derive newtype instance Eq ScriptDataHash
derive newtype instance EncodeAeson ScriptDataHash

instance Show ScriptDataHash where
  show = genericShow

newtype Mint = Mint NonAdaAsset

derive instance Generic Mint _
derive instance Newtype Mint _
derive newtype instance Eq Mint
derive newtype instance Semigroup Mint
derive newtype instance Monoid Mint
derive newtype instance EncodeAeson Mint

instance Show Mint where
  show = genericShow

newtype AuxiliaryDataHash = AuxiliaryDataHash ByteArray

derive instance Generic AuxiliaryDataHash _
derive instance Newtype AuxiliaryDataHash _
derive newtype instance Eq AuxiliaryDataHash
derive newtype instance EncodeAeson AuxiliaryDataHash

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

instance EncodeAeson ProposedProtocolParameterUpdates where
  encodeAeson (ProposedProtocolParameterUpdates r) = encodeMap r

newtype GenesisHash = GenesisHash ByteArray

derive instance Newtype GenesisHash _
derive newtype instance Eq GenesisHash
derive newtype instance Ord GenesisHash
derive instance Generic GenesisHash _
derive newtype instance EncodeAeson GenesisHash

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
  , protocolVersion :: Maybe ProtocolVersion
  , minPoolCost :: Maybe BigNum
  , adaPerUtxoByte :: Maybe BigNum
  , costModels :: Maybe Costmdls
  , executionCosts :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe UInt
  , collateralPercentage :: Maybe UInt
  , maxCollateralInputs :: Maybe UInt
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

instance EncodeAeson Costmdls where
  encodeAeson = encodeMap <<< unwrap

newtype CostModel = CostModel (Array Int.Int)

derive instance Newtype CostModel _
derive newtype instance Eq CostModel
derive newtype instance EncodeAeson CostModel
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

instance DecodeAeson Nonce where
  decodeAeson aeson = (HashNonce <$> decodeAeson aeson) <|>
    caseAesonString err
      ( case _ of
          "neutral" -> pure IdentityNonce
          _ -> err
      )
      aeson
    where
    err :: Either JsonDecodeError Nonce
    err = Left (TypeMismatch "Nonce")

instance EncodeAeson Nonce where
  encodeAeson IdentityNonce = encodeAeson "neutral"
  encodeAeson (HashNonce hash) = encodeAeson hash

type UnitInterval =
  { numerator :: BigNum
  , denominator :: BigNum
  }

newtype Epoch = Epoch UInt

derive instance Newtype Epoch _
derive instance Generic Epoch _
derive newtype instance Eq Epoch
derive newtype instance EncodeAeson Epoch

instance Show Epoch where
  show = genericShow

newtype Ipv4 = Ipv4 ByteArray

derive instance Eq Ipv4
derive instance Generic Ipv4 _
derive instance Newtype Ipv4 _
-- TODO: Use a more legible representation
derive newtype instance EncodeAeson Ipv4

instance Show Ipv4 where
  show = genericShow

newtype Ipv6 = Ipv6 ByteArray

derive instance Eq Ipv6
derive instance Generic Ipv6 _
derive instance Newtype Ipv6 _
derive newtype instance EncodeAeson Ipv6

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

instance EncodeAeson Relay where
  encodeAeson = case _ of
    SingleHostAddr r -> encodeTagged' "SingleHostAddr" r
    SingleHostName r -> encodeTagged' "SingleHostName" r
    MultiHostName r -> encodeTagged' "MultiHostName" r

newtype URL = URL String

derive instance Eq URL
derive instance Generic URL _
derive instance Newtype URL _
derive newtype instance EncodeAeson URL

instance Show URL where
  show = genericShow

newtype PoolMetadataHash = PoolMetadataHash ByteArray

derive instance Eq PoolMetadataHash
derive instance Generic PoolMetadataHash _
derive instance Newtype PoolMetadataHash _
derive newtype instance EncodeAeson PoolMetadataHash

instance Show PoolMetadataHash where
  show = genericShow

newtype PoolMetadata = PoolMetadata
  { url :: URL
  , hash :: PoolMetadataHash
  }

derive instance Eq PoolMetadata
derive instance Generic PoolMetadata _
derive newtype instance EncodeAeson PoolMetadata

instance Show PoolMetadata where
  show = genericShow

newtype GenesisDelegateHash = GenesisDelegateHash ByteArray

derive instance Eq GenesisDelegateHash
derive instance Generic GenesisDelegateHash _
derive newtype instance EncodeAeson GenesisDelegateHash

instance Show GenesisDelegateHash where
  show = genericShow

newtype MIRToStakeCredentials = MIRToStakeCredentials
  (Map StakeCredential Int.Int)

derive instance Eq MIRToStakeCredentials
derive instance Generic MIRToStakeCredentials _

instance Show MIRToStakeCredentials where
  show = genericShow

instance EncodeAeson MIRToStakeCredentials where
  encodeAeson (MIRToStakeCredentials r) = encodeMap r

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

instance EncodeAeson MoveInstantaneousReward where
  encodeAeson = case _ of
    ToOtherPot r -> encodeTagged' "ToOtherPot" r
      -- We assume the numbers are finite
      { pot = unsafePartial $ fromJust $ finiteNumber r.pot }
    ToStakeCreds r -> encodeTagged' "ToStakeCreds" r
      -- We assume the numbers are finite
      { pot = unsafePartial $ fromJust $ finiteNumber r.pot }

type PoolRegistrationParams =
  { operator :: PoolPubKeyHash -- cwitness (cert)
  , vrfKeyhash :: VRFKeyHash
  -- needed to prove that the pool won the lottery
  , pledge :: BigNum
  , cost :: BigNum -- >= pparams.minPoolCost
  , margin :: UnitInterval -- proportion that goes to the reward account
  , rewardAccount :: RewardAddress
  , poolOwners :: Array PaymentPubKeyHash
  -- payment key hashes that contribute to pledge amount
  , relays :: Array Relay
  , poolMetadata :: Maybe PoolMetadata
  }

newtype PoolPubKeyHash = PoolPubKeyHash PubKeyHash

derive instance Newtype PoolPubKeyHash _
derive instance Eq PoolPubKeyHash
derive instance Ord PoolPubKeyHash
derive instance Generic PoolPubKeyHash _
derive newtype instance ToData PoolPubKeyHash
derive newtype instance FromData PoolPubKeyHash

instance EncodeAeson PoolPubKeyHash where
  encodeAeson (PoolPubKeyHash kh) =
    encodeAeson (ed25519KeyHashToBech32 "pool" $ unwrap kh)

instance DecodeAeson PoolPubKeyHash where
  decodeAeson aeson = do
    str <- decodeAeson aeson
    PoolPubKeyHash <<< PubKeyHash <$> note (TypeMismatch "PoolPubKeyHash")
      (ed25519KeyHashFromBech32 str)

instance Show PoolPubKeyHash where
  show (PoolPubKeyHash kh) =
    "(PoolPubKeyHash (Ed25519KeyHash (unsafePartial $ fromJust $ \
    \ed25519KeyHashFromBech32 "
      <> show (ed25519KeyHashToBech32 "pool" $ unwrap kh)
      <> ")))"

mkPoolPubKeyHash :: Bech32String -> Maybe PoolPubKeyHash
mkPoolPubKeyHash str
  | startsWith "pool" str = PoolPubKeyHash <<< PubKeyHash <$>
      ed25519KeyHashFromBech32 str
  | otherwise = Nothing

poolPubKeyHashToBech32 :: PoolPubKeyHash -> Bech32String
poolPubKeyHashToBech32 = unwrap >>> unwrap >>> ed25519KeyHashToBech32Unsafe
  "pool"

data Certificate
  = StakeRegistration StakeCredential
  | StakeDeregistration StakeCredential
  | StakeDelegation StakeCredential PoolPubKeyHash
  | PoolRegistration PoolRegistrationParams
  | PoolRetirement
      { poolKeyHash :: PoolPubKeyHash
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

instance EncodeAeson Certificate where
  encodeAeson = case _ of
    StakeRegistration r -> encodeTagged' "StakeRegistration" r
    StakeDeregistration r -> encodeTagged' "StakeDeregistration"
      r
    StakeDelegation cred hash -> encodeTagged' "StakeDelegation"
      { stakeCredential: cred, ed25519KeyHash: hash }
    PoolRegistration r -> encodeTagged' "PoolRegistration" r
    PoolRetirement r -> encodeTagged' "PoolRetirement" r
    GenesisKeyDelegation r -> encodeTagged'
      "GenesisKeyDelegation"
      r
    MoveInstantaneousRewardsCert r -> encodeTagged'
      "MoveInstantaneousReward"
      r

--------------------------------------------------------------------------------
-- `TxBody` Lenses
--------------------------------------------------------------------------------

_inputs :: Lens' TxBody (Set TransactionInput)
_inputs = _Newtype <<< prop (Proxy :: Proxy "inputs")

_outputs :: Lens' TxBody (Array TransactionOutput)
_outputs = _Newtype <<< prop (Proxy :: Proxy "outputs")

_fee :: Lens' TxBody (Coin)
_fee = _Newtype <<< prop (Proxy :: Proxy "fee")

_ttl :: Lens' TxBody (Maybe Slot)
_ttl = _Newtype <<< prop (Proxy :: Proxy "ttl")

_certs :: Lens' TxBody (Maybe (Array Certificate))
_certs = _Newtype <<< prop (Proxy :: Proxy "certs")

_withdrawals :: Lens' TxBody (Maybe (Map RewardAddress Coin))
_withdrawals = _Newtype <<< prop (Proxy :: Proxy "withdrawals")

_update :: Lens' TxBody (Maybe Update)
_update = _Newtype <<< prop (Proxy :: Proxy "update")

_auxiliaryDataHash :: Lens' TxBody (Maybe AuxiliaryDataHash)
_auxiliaryDataHash = _Newtype <<< prop (Proxy :: Proxy "auxiliaryDataHash")

_validityStartInterval :: Lens' TxBody (Maybe Slot)
_validityStartInterval =
  _Newtype <<< prop (Proxy :: Proxy "validityStartInterval")

_mint :: Lens' TxBody (Maybe Mint)
_mint = _Newtype <<< prop (Proxy :: Proxy "mint")

_scriptDataHash :: Lens' TxBody (Maybe ScriptDataHash)
_scriptDataHash = _Newtype <<< prop (Proxy :: Proxy "scriptDataHash")

_collateral :: Lens' TxBody (Maybe (Array TransactionInput))
_collateral = _Newtype <<< prop (Proxy :: Proxy "collateral")

_requiredSigners :: Lens' TxBody (Maybe (Array RequiredSigner))
_requiredSigners = _Newtype <<< prop (Proxy :: Proxy "requiredSigners")

_networkId :: Lens' TxBody (Maybe NetworkId)
_networkId = _Newtype <<< prop (Proxy :: Proxy "networkId")

_referenceInputs :: Lens' TxBody (Set TransactionInput)
_referenceInputs = _Newtype <<< prop (Proxy :: Proxy "referenceInputs")

_collateralReturn :: Lens' TxBody (Maybe TransactionOutput)
_collateralReturn = _Newtype <<< prop (Proxy :: Proxy "collateralReturn")

_totalCollateral :: Lens' TxBody (Maybe Coin)
_totalCollateral = _Newtype <<< prop (Proxy :: Proxy "totalCollateral")

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
derive newtype instance EncodeAeson TransactionWitnessSet

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
derive newtype instance EncodeAeson RequiredSigner
derive instance Generic RequiredSigner _

instance Show RequiredSigner where
  show = genericShow

newtype Vkeywitness = Vkeywitness (Vkey /\ Ed25519Signature)

derive instance Generic Vkeywitness _
derive newtype instance Eq Vkeywitness
derive newtype instance EncodeAeson Vkeywitness
derive instance Newtype Vkeywitness _

instance Show Vkeywitness where
  show = genericShow

newtype Vkey = Vkey PublicKey

derive instance Generic Vkey _
derive instance Newtype Vkey _
derive newtype instance Eq Vkey
derive newtype instance Ord Vkey
derive newtype instance EncodeAeson Vkey

instance Show Vkey where
  show = genericShow

newtype PublicKey = PublicKey RawBytes

mkPublicKey :: Bech32String -> Maybe PublicKey
mkPublicKey = map (PublicKey <<< bytesFromPublicKey) <<< publicKeyFromBech32

mkFromCslPubKey :: Serialization.PublicKey -> PublicKey
mkFromCslPubKey = PublicKey <<< bytesFromPublicKey

convertPubKey :: PublicKey -> Serialization.PublicKey
convertPubKey (PublicKey bs) = unsafePartial
  $ fromJust <<< fromBytes <<< wrap <<< unwrap
  $ bs

derive newtype instance Eq PublicKey
derive newtype instance Ord PublicKey
derive newtype instance EncodeAeson PublicKey

instance ToData PublicKey where
  toData (PublicKey bytes) = toData bytes

instance FromData PublicKey where
  fromData = map mkFromCslPubKey <<< fromBytes <=< fromData

instance Show PublicKey where
  show pk = "(PublicKey " <> (bech32FromPublicKey <<< convertPubKey $ pk) <> ")"

newtype Ed25519Signature = Ed25519Signature RawBytes

mkEd25519Signature :: Bech32String -> Maybe Ed25519Signature
mkEd25519Signature =
  map (Ed25519Signature <<< wrap <<< unwrap <<< toBytes) <<<
    ed25519SignatureFromBech32

mkFromCslEd25519Signature :: Serialization.Ed25519Signature -> Ed25519Signature
mkFromCslEd25519Signature = Ed25519Signature <<< wrap <<< unwrap <<< toBytes

convertEd25519Signature :: Ed25519Signature -> Serialization.Ed25519Signature
convertEd25519Signature (Ed25519Signature bs) = unsafePartial
  $ fromJust <<< fromBytes <<< wrap <<< unwrap
  $ bs

derive newtype instance Eq Ed25519Signature
derive newtype instance Ord Ed25519Signature
derive newtype instance EncodeAeson Ed25519Signature

instance Show Ed25519Signature where
  show sig = "(Ed25519Signature "
    <> (bech32FromEd25519Signature <<< convertEd25519Signature $ sig)
    <> ")"

newtype Redeemer = Redeemer
  { tag :: RedeemerTag
  , index :: BigInt
  , data :: PlutusData
  , exUnits :: ExUnits
  }

derive instance Generic Redeemer _
derive instance Newtype Redeemer _
derive newtype instance Eq Redeemer
derive newtype instance Ord Redeemer
derive newtype instance EncodeAeson Redeemer

instance Show Redeemer where
  show = genericShow

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , nativeScripts :: Maybe (Array NativeScript)
  , plutusScripts :: Maybe (Array PlutusScript)
  }

derive instance Generic AuxiliaryData _
derive instance Newtype AuxiliaryData _
derive newtype instance Eq AuxiliaryData
derive newtype instance EncodeAeson AuxiliaryData

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

newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , scriptRef :: Maybe ScriptRef
  }

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput
derive newtype instance EncodeAeson TransactionOutput

instance Show TransactionOutput where
  show = genericShow

type UtxoMap = Map TransactionInput TransactionOutput

pprintUtxoMap :: UtxoMap -> TagSet
pprintUtxoMap utxos = TagSet.fromArray $
  Map.toUnfoldable utxos <#>
    \( TransactionInput { transactionId, index } /\
         TransactionOutput { address, amount, datum, scriptRef }
     ) ->
      let
        datumTagSets = case datum of
          NoOutputDatum -> []
          OutputDatumHash datumHash ->
            [ TagSet.fromArray
                [ "datum hash" `tag` byteArrayToHex (unwrap datumHash) ]
            ]
          OutputDatum plutusData ->
            [ TagSet.fromArray
                [ "datum" `tagSetTag` pprintPlutusData (unwrap plutusData) ]
            ]
        scriptRefTagSets = case scriptRef of
          Nothing -> []
          Just ref -> [ "Script Reference" `tag` show ref ]
        outputTagSet =
          [ "amount" `tagSetTag` pprintValue amount
          , "address" `tag` addressBech32 address
          ]
            <> datumTagSets
            <> scriptRefTagSets
      in
        (byteArrayToHex (unwrap transactionId) <> "#" <> UInt.toString index)
          `tagSetTag` TagSet.fromArray outputTagSet
