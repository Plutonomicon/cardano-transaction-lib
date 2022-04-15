module Deserialization.Transaction where

import Prelude

import Contract.Address (NetworkId, RequiredSigner, Slot(..), StakeCredential)
import Contract.Prelude (Tuple, traverse)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (Ed25519KeyHash)
import Contract.Transaction
  ( AuxiliaryData(..)
  , AuxiliaryDataHash
  , Certificate
  , Epoch(..)
  , GeneralTransactionMetadata
  , GenesisHash
  , Mint(..)
  , ProposedProtocolParameterUpdates(..)
  , ProtocolParamUpdate
  , ScriptDataHash
  , TxBody(..)
  , Update
  )
import Contract.Value (Coin(..), NonAdaAsset(..), TokenName)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as M
import Data.Maybe (Maybe)
import Data.UInt as UInt
import Deserialization.BigNum (bigNumToBigInt)
import Deserialization.Error (FromCslRepError, fromCslRepError)
import Deserialization.FromBytes (FromBytesError, fromBytes')
import Deserialization.UnspentOutput (convertInput, convertOutput)
import Deserialization.WitnessSet
  ( convertNativeScripts
  , convertPlutusScripts
  , convertWitnessSet
  )
import Error (E, NotImplementedError, notImplementedError, noteE)
import FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import Helpers (notImplemented)
import Serialization.Address (RewardAddress, intToNetworkId)
import Serialization.Hash (ScriptHash)
import Serialization.Types (NativeScripts, PlutusScripts)
import Serialization.Types as CSL
import Type.Row (type (+))
import Types.Transaction as T
import Types.Value (scriptHashAsCurrencySymbol, tokenNameFromAssetName)

type Err r a = E (FromBytesError + NotImplementedError + FromCslRepError + r) a

noteErr = noteE <<< fromCslRepError

deserializeTransaction
  :: forall r. { txCbor :: ByteArray } -> Err r T.Transaction
deserializeTransaction { txCbor } = fromBytes' txCbor >>= convertTransaction

convertTransaction :: forall r. CSL.Transaction -> Err r T.Transaction
convertTransaction tx = do
  witnessSet <- noteErr "convertWitnessSet" $ convertWitnessSet
    (_txWitnessSet tx)
  body <- convertTxBody $ _txBody tx
  pure $ T.Transaction
    { body
    , witnessSet
    , isValid: _txIsValid tx
    , auxiliaryData: convertAuxiliaryData <$> _txAuxiliaryData maybeFfiHelper tx
    }

convertTxBody :: forall r. CSL.TransactionBody -> Err r TxBody
convertTxBody txBody = do
  inputs <-
    _txBodyInputs containerHelper txBody
      # traverse (convertInput >>> noteErr "TransactionInput")
  outputs <-
    _txBodyOutputs containerHelper txBody
      # traverse (convertOutput >>> noteErr "TransactionOutput")
  fee <-
    Coin <$>
      ( _txBodyFee txBody
          # bigNumToBigInt >>> noteErr "TxBodyFee to BigInt"
      )
  networkId <-
    _txBodyNetworkId maybeFfiHelper txBody
      # traverse (intToNetworkId >>> noteErr "NetworkId")
  let
    ws = _unpackWithdrawals containerHelper <$> _txBodyWithdrawals
      maybeFfiHelper
      txBody
    bignumToCoin = (pure <<< Coin) <=< bigNumToBigInt
    withdrawals = map M.fromFoldable <<< traverse (traverse bignumToCoin) =<< ws

  update <- traverse convertUpdate $ _txBodyUpdate maybeFfiHelper txBody

  certs <- traverse (traverse convertCert) $ _txBodyCerts containerHelper
    maybeFfiHelper
    txBody

  validityStartInterval <-
    traverse intToSlot $ _txBodyValidityStartInterval maybeFfiHelper txBody

  pure $ TxBody
    { inputs
    , outputs
    , fee
    , ttl: map Slot <<< UInt.fromNumber' =<< _txBodyTtl maybeFfiHelper txBody
    , certs
    , withdrawals
    , update
    , auxiliaryDataHash: _txBodyAuxiliaryDataHash maybeFfiHelper txBody
    , validityStartInterval
    , mint: map convertMint $ _txBodyMint maybeFfiHelper txBody
    , scriptDataHash: _txBodyScriptDataHash maybeFfiHelper txBody
    , collateral: _txBodyCollateral containerHelper maybeFfiHelper txBody >>=
        traverse convertInput
    , requiredSigners: _txBodyRequiredSigners maybeFfiHelper txBody #
        (map <<< map) convertRequiredSigner
    , networkId
    }

  where
  intToSlot x =
    noteErr ("validityStartInterval UInt.fromInt': " <> show x)
      <<< map Slot
      <<< UInt.fromInt' $ x

convertRequiredSigner :: Ed25519KeyHash -> RequiredSigner
convertRequiredSigner = notImplemented -- TODO: how?

convertMint :: CSL.Mint -> Mint
convertMint mint = Mint $ NonAdaAsset
  $
    -- outer map
    M.fromFoldable <<< map (lmap scriptHashAsCurrencySymbol)
      -- inner map
      <<< (map <<< map)
        ( M.fromFoldable <<< map convAssetName <<< _unpackMintAssets
            containerHelper
        )
  $ _unpackMint containerHelper mint

  where
  convAssetName :: Tuple CSL.AssetName Int -> Tuple TokenName BigInt
  convAssetName = bimap tokenNameFromAssetName BigInt.fromInt

convertUpdate :: forall r. CSL.Update -> Err r Update
convertUpdate = _unpackUpdate containerHelper >>> \{ epoch, paramUpdates } -> do
  epoch' <- Epoch <$>
    (noteErr ("fromInt epoch: " <> show epoch) $ UInt.fromInt' epoch)
  let
    convertUpdates = ProposedProtocolParameterUpdates <<< M.fromFoldable <<< map
      (map convertProtocolParamUpdate)
  pure
    { epoch: epoch'
    , proposedProtocolParameterUpdates: convertUpdates paramUpdates
    }

convertProtocolParamUpdate :: CSL.ProtocolParamUpdate -> ProtocolParamUpdate
convertProtocolParamUpdate = notImplemented

convertAuxiliaryData :: CSL.AuxiliaryData -> AuxiliaryData
convertAuxiliaryData ad =
  AuxiliaryData
    { metadata: convertGeneralTransactionMetadata <$> _adGeneralMetadata
        maybeFfiHelper
        ad
    , nativeScripts: convertNativeScripts =<< _adNativeScripts maybeFfiHelper ad
    , plutusScripts: convertPlutusScripts <$> _adPlutusScripts maybeFfiHelper ad
    }

convertGeneralTransactionMetadata
  :: CSL.GeneralTransactionMetadata -> GeneralTransactionMetadata
convertGeneralTransactionMetadata = notImplemented -- TODO

-- | NOTE partially implemented
convertCert :: forall r. CSL.Certificate -> Err r Certificate
convertCert = _convertCert certConvHelper

---- foreign imports

foreign import _txBody :: CSL.Transaction -> CSL.TransactionBody
foreign import _txIsValid :: CSL.Transaction -> Boolean
foreign import _txWitnessSet :: CSL.Transaction -> CSL.TransactionWitnessSet
foreign import _txAuxiliaryData
  :: MaybeFfiHelper -> CSL.Transaction -> Maybe CSL.AuxiliaryData

foreign import _adGeneralMetadata
  :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe CSL.GeneralTransactionMetadata

foreign import _adNativeScripts
  :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe NativeScripts

foreign import _adPlutusScripts
  :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe PlutusScripts

-- inputs(): TransactionInputs;
foreign import _txBodyInputs
  :: ContainerHelper -> CSL.TransactionBody -> Array CSL.TransactionInput

-- outputs(): TransactionOutputs;
foreign import _txBodyOutputs
  :: ContainerHelper -> CSL.TransactionBody -> Array CSL.TransactionOutput

-- fee(): BigNum;
foreign import _txBodyFee :: CSL.TransactionBody -> CSL.BigNum
-- ttl(): number | void;
foreign import _txBodyTtl
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Number

-- certs(): Certificates | void;
foreign import _txBodyCerts
  :: ContainerHelper
  -> MaybeFfiHelper
  -> CSL.TransactionBody
  -> Maybe (Array CSL.Certificate)

-- withdrawals(): Withdrawals | void
foreign import _txBodyWithdrawals
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Withdrawals

-- update(): Update | void
foreign import _txBodyUpdate
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Update

-- auxiliary_data_hash(): AuxiliaryDataHash | void
foreign import _txBodyAuxiliaryDataHash
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe AuxiliaryDataHash

-- validity_start_interval(): number | void
foreign import _txBodyValidityStartInterval
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Int

-- mint(): Mint | void
foreign import _txBodyMint
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Mint

-- multiassets(): Mint | void
foreign import _txBodyMultiAssets
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Mint

-- script_data_hash(): ScriptDataHash | void
foreign import _txBodyScriptDataHash
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe ScriptDataHash

-- collateral(): TransactionInputs | void
foreign import _txBodyCollateral
  :: ContainerHelper
  -> MaybeFfiHelper
  -> CSL.TransactionBody
  -> Maybe (Array CSL.TransactionInput)

-- required_signers(): Ed25519KeyHashes | void
foreign import _txBodyRequiredSigners
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe (Array Ed25519KeyHash)

-- network_id(): NetworkId | void
foreign import _txBodyNetworkId
  :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Int

foreign import _unpackWithdrawals
  :: ContainerHelper
  -> CSL.Withdrawals
  -> Array (Tuple RewardAddress CSL.BigNum)

foreign import _unpackUpdate
  :: ContainerHelper
  -> CSL.Update
  -> { epoch :: Int
     , paramUpdates :: Array (Tuple GenesisHash CSL.ProtocolParamUpdate)
     }

foreign import _unpackMint
  :: ContainerHelper -> CSL.Mint -> Array (Tuple ScriptHash CSL.MintAssets)

foreign import _unpackMintAssets
  :: ContainerHelper -> CSL.MintAssets -> Array (Tuple CSL.AssetName Int)

---- internal helpers
type CertConvHelper r =
  { stakeDeregistration :: StakeCredential -> Err r Certificate
  , stakeRegistration :: StakeCredential -> Err r Certificate
  , stakeDelegation ::
      StakeCredential -> Ed25519KeyHash -> Err r Certificate
  , notImplementedError :: String -> Err r Certificate
  -- , poolRegistration
  -- , poolRetirement
  -- , genesisKeyDelegation
  -- , moveInstantaneousRewardsCert
  }

certConvHelper :: forall r. CertConvHelper r
certConvHelper = notImplemented

foreign import _convertCert
  :: forall r. CertConvHelper r -> CSL.Certificate -> Err r Certificate

-- = StakeRegistration StakeCredential
-- | StakeDeregistration StakeCredential
-- | StakeDelegation StakeCredential Ed25519KeyHash
-- | PoolRegistration
--     { operator :: Ed25519KeyHash
--     , vrfKeyhash :: VRFKeyHash
--     , pledge :: BigNum
--     , cost :: BigNum
--     , margin :: UnitInterval
--     , reward_account :: RewardAddress
--     , poolOwners :: Array Ed25519KeyHash
--     , relays :: Array Relay
--     , poolMetadata :: Maybe PoolMetadata
--     }
-- | PoolRetirement
--     { poolKeyhash :: Ed25519KeyHash
--     , epoch :: Epoch
--     }
-- | GenesisKeyDelegation
--     { genesisHash :: GenesisHash
--     , genesisDelegateHash :: GenesisDelegateHash
--     , vrfKeyhash :: VRFKeyHash
--     }
-- | MoveInstantaneousRewardsCert MoveInstantaneousReward

-- declare export class Certificate {
--   kind(): number;
--   as_stake_registration(): StakeRegistration | void;
--   as_stake_deregistration(): StakeDeregistration | void;
--   as_stake_delegation(): StakeDelegation | void;
--   as_pool_registration(): PoolRegistration | void;
--   as_pool_retirement(): PoolRetirement | void;
--   as_genesis_key_delegation(): GenesisKeyDelegation | void;
--   as_move_instantaneous_rewards_cert(): MoveInstantaneousRewardsCert | void;
-- }
