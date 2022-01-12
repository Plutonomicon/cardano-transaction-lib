module Types.Transaction where

import Prelude
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Foldable (any)
import Data.Maybe (Maybe)
import Data.These (These(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
derive instance newtypeTxBody :: Newtype TxBody _

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
derive instance eqCurrencySymbol :: Eq CurrencySymbol
derive instance ordCurrencySymbol :: Ord CurrencySymbol
derive instance genericCurrencySymbol :: Generic CurrencySymbol _

instance showCurrencySymbol :: Show CurrencySymbol where
  show = genericShow

newtype TokenName = TokenName String
derive instance eqTokenName :: Eq TokenName
derive instance ordTokenName :: Ord TokenName
derive instance genericTokenName :: Generic TokenName _

instance showTokenName :: Show TokenName where
  show = genericShow

newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt))
derive instance eqValue :: Eq Value
derive instance genericValue :: Generic Value _
derive instance newtypeValue :: Newtype Value _

instance showValue :: Show Value where
  show = genericShow

instance semigroupValue :: Semigroup Value where
  append = unionWith (+)
  -- append v1 v2 =
  --   Value $ Map.unionWith (Map.unionWith (+)) (unwrap v1) (unwrap v2)

instance monoidValue :: Monoid Value where
  mempty = Value Map.empty

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.AssocMap.html#union
-- | Combine two 'Map's.
union :: ∀ k v r. Ord k => Map k v -> Map k r -> Map k (These v r)
union l r =
  let ls :: Array (k /\ v)
      ls = Map.toUnfoldable l

      rs :: Array (k /\ r)
      rs = Map.toUnfoldable r

      f :: v -> Maybe r -> These v r
      f a b' = case b' of
          Nothing -> This a
          Just b  -> Both a b

      ls' :: Array (k /\ These v r)
      ls' = map (\(c /\ i) -> (c /\ f i (Map.lookup c (Map.fromFoldable rs)))) ls

      rs' :: Array (k /\ r)
      rs' = Array.filter (\(c /\ _) -> not (any (\(c' /\ _) -> c' == c) ls)) rs

      rs'' :: Array (k /\ These v r)
      rs'' = map (map That) rs'
   in Map.fromFoldable (ls' <> rs'')

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- | Combine two 'Value' maps
unionVal
  :: Value
  -> Value
  -> Map CurrencySymbol (Map TokenName (These BigInt BigInt))
unionVal (Value l) (Value r) =
  let combined = union l r
      unBoth k = case k of
        This a -> This <$> a
        That b -> That <$> b
        Both a b -> union a b
   in unBoth <$> combined

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
unionWith
  :: (BigInt -> BigInt -> BigInt)
  -> Value
  -> Value
  -> Value
unionWith f ls rs =
  let combined = unionVal ls rs
      unBoth k' = case k' of
        This a -> f a zero
        That b -> f zero b
        Both a b -> f a b
   in Value (map (map unBoth) combined)

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
derive instance ordTransactionInput :: Ord TransactionInput

newtype TransactionOutput = TransactionOutput
  { address :: Address,
    amount :: Value,
    data_hash :: Maybe String -- DataHash>,
  }
derive instance newtypeTransactionOutput :: Newtype TransactionOutput _

type Utxo = Map TransactionInput TransactionOutput

newtype Coin = Coin BigInt

newtype Slot = Slot BigInt

newtype Address = Address
  { "AddrType" :: BaseAddress
  }
derive instance newtypeAddress :: Newtype Address _

newtype BaseAddress = BaseAddress
  { network :: Int, -- u8,
    stake :: Credential,
    payment :: Credential
  }
derive instance newtypeBaseAddress :: Newtype BaseAddress _

newtype Credential = Credential String
derive instance eqCredential :: Eq Credential
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
