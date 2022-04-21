module Serialization.Types
  ( AssetName
  , Assets
  , AuxiliaryData
  , AuxiliaryDataHash
  , BigInt
  , BigNum
  , Bip32PublicKey
  , BootstrapWitness
  , BootstrapWitnesses
  , Certificate
  , Certificates
  , ConstrPlutusData
  , CostModel
  , Costmdls
  , DataHash
  , Ed25519KeyHashes
  , Ed25519Signature
  , ExUnits
  , GeneralTransactionMetadata
  , GenesisDelegateHash
  , GenesisHash
  , Int32
  , Ipv4
  , Ipv6
  , Language
  , MIRToStakeCredentials
  , MetadataList
  , MetadataMap
  , Mint
  , MintAssets
  , MoveInstantaneousReward
  , MultiAsset
  , NativeScript
  , NativeScripts
  , NetworkId
  , Nonce
  , PlutusData
  , PlutusList
  , PlutusMap
  , PlutusScript
  , PlutusScripts
  , PoolMetadata
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , PublicKey
  , Redeemer
  , RedeemerTag
  , Redeemers
  , Relay
  , Relays
  , ScriptAll
  , ScriptAny
  , ScriptDataHash
  , ScriptNOfK
  , ScriptPubkey
  , TimelockExpiry
  , TimelockStart
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionMetadatum
  , TransactionOutput
  , TransactionOutputs
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , UnitInterval
  , Update
  , VRFKeyHash
  , Value
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  , Withdrawals
  ) where

import Prelude
import Data.Function (on)

foreign import data AssetName :: Type
foreign import data Assets :: Type
foreign import data AuxiliaryData :: Type
foreign import data AuxiliaryDataHash :: Type
foreign import data BigInt :: Type
foreign import data BigNum :: Type
foreign import data Bip32PublicKey :: Type
foreign import data BootstrapWitness :: Type
foreign import data BootstrapWitnesses :: Type
foreign import data Certificate :: Type
foreign import data Certificates :: Type
foreign import data ConstrPlutusData :: Type
foreign import data CostModel :: Type
foreign import data Costmdls :: Type
foreign import data DataHash :: Type
foreign import data Ed25519KeyHashes :: Type
foreign import data Ed25519Signature :: Type
foreign import data ExUnits :: Type
foreign import data GeneralTransactionMetadata :: Type
foreign import data GenesisDelegateHash :: Type
foreign import data GenesisHash :: Type
foreign import data Int32 :: Type
foreign import data Ipv4 :: Type
foreign import data Ipv6 :: Type
foreign import data Language :: Type
foreign import data MIRToStakeCredentials :: Type
foreign import data MetadataList :: Type
foreign import data MetadataMap :: Type
foreign import data Mint :: Type
foreign import data MintAssets :: Type
foreign import data MoveInstantaneousReward :: Type
foreign import data MultiAsset :: Type
foreign import data NativeScript :: Type
foreign import data NativeScripts :: Type
foreign import data NetworkId :: Type
foreign import data Nonce :: Type
foreign import data PlutusData :: Type
foreign import data PlutusList :: Type
foreign import data PlutusMap :: Type
foreign import data PlutusScript :: Type
foreign import data PlutusScripts :: Type
foreign import data PoolMetadata :: Type
foreign import data ProposedProtocolParameterUpdates :: Type
foreign import data ProtocolParamUpdate :: Type
foreign import data PublicKey :: Type
foreign import data Redeemer :: Type
foreign import data RedeemerTag :: Type
foreign import data Redeemers :: Type
foreign import data Relay :: Type
foreign import data Relays :: Type
foreign import data ScriptAll :: Type
foreign import data ScriptAny :: Type
foreign import data ScriptDataHash :: Type
foreign import data ScriptNOfK :: Type
foreign import data ScriptPubkey :: Type
foreign import data TimelockExpiry :: Type
foreign import data TimelockStart :: Type
foreign import data Transaction :: Type
foreign import data TransactionBody :: Type
foreign import data TransactionHash :: Type
foreign import data TransactionInput :: Type
foreign import data TransactionInputs :: Type
foreign import data TransactionMetadatum :: Type
foreign import data TransactionOutput :: Type
foreign import data TransactionOutputs :: Type
foreign import data TransactionUnspentOutput :: Type
foreign import data TransactionWitnessSet :: Type
foreign import data UnitInterval :: Type
foreign import data Update :: Type
foreign import data VRFKeyHash :: Type
foreign import data Value :: Type
foreign import data Vkey :: Type
foreign import data Vkeywitness :: Type
foreign import data Vkeywitnesses :: Type
foreign import data Withdrawals :: Type

instance Show BigNum where
  show = _to_str

instance Eq BigNum where
  eq = eq `on` show

instance Show VRFKeyHash where
  show = _to_bech32

instance Eq VRFKeyHash where
  eq = eq `on` show

foreign import _to_str :: forall a. a -> String
foreign import _to_bech32 :: forall a. a -> String
