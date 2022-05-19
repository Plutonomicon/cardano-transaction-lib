-- | A module for shared types across Cardano.Types.Transaction and
-- | Plutus.Types.Transaction.
module Types.Transaction
  ( DataHash(..)
  , TransactionHash(..)
  , TransactionInput(..)
  , TxOutput(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Types.ByteArray (ByteArray, byteArrayToHex)
import Types.PlutusData (PlutusData(Constr))

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

-- | Newtype for all `TransactionOutput` types, this wraps the separate `Cardano`
-- | and `Plutus` types
newtype TxOutput address value = TxOutput
  { address :: address
  , amount :: value
  , dataHash :: Maybe DataHash
  }

derive instance Generic (TxOutput a v) _
derive instance Newtype (TxOutput a v) _
derive newtype instance (Eq a, Eq v) => Eq (TxOutput a v)

instance (Show a, Show v) => Show (TxOutput a v) where
  show = genericShow

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
