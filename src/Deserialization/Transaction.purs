module Deserialization.Transaction where

import Prelude

import Contract.Address (RequiredSigner, Slot(..))
import Contract.Prelude (Tuple(..), traverse)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (Ed25519KeyHash)
import Contract.Transaction (AuxiliaryData(..), AuxiliaryDataHash, Certificate, Epoch(..), GeneralTransactionMetadata, GenesisHash, Mint, ProposedProtocolParameterUpdates(..), ProtocolParamUpdate, ScriptDataHash, TxBody(..), Update)
import Contract.Value (Coin(..))
import Data.Map as M
import Data.Maybe (Maybe)
import Data.UInt as UInt
import Deserialization.BigNum (bigNumToBigInt)
import Deserialization.FromBytes (fromBytes)
import Deserialization.UnspentOutput (convertInput, convertOutput)
import Deserialization.WitnessSet (convertNativeScripts, convertPlutusScripts, convertWitnessSet)
import FfiHelpers (ContainerHelper, MaybeFfiHelper, containerHelper, maybeFfiHelper)
import Helpers (notImplemented)
import Serialization.Address (RewardAddress, intToNetworkId)
import Serialization.Types (NativeScripts, PlutusScripts)
import Serialization.Types as CSL
import Types.Transaction as T


deserializeTransaction :: { txCbor :: ByteArray } -> Maybe T.Transaction
deserializeTransaction { txCbor } = fromBytes txCbor >>= convertTransaction

convertTransaction :: CSL.Transaction -> Maybe T.Transaction
convertTransaction tx = do
  witnessSet <- convertWitnessSet (_txWitnessSet tx)
  body <- convertTxBody $ _txBody tx
  pure $ T.Transaction
   { body
   , witnessSet
   , isValid: _txIsValid tx
   , auxiliaryData: convertAuxiliaryData <$> _txAuxiliaryData maybeFfiHelper tx
   }

convertTxBody :: CSL.TransactionBody -> Maybe TxBody
convertTxBody txBody = do
  inputs <- traverse convertInput $ _txBodyInputs containerHelper txBody
  outputs <- traverse convertOutput $ _txBodyOutputs containerHelper txBody
  fee <- Coin <$> bigNumToBigInt (_txBodyFee txBody)
  networkId <- intToNetworkId <$> _txBodyNetworkId maybeFfiHelper txBody
  let ws = _unpackWithdrawals Tuple <$> _txBodyWithdrawals maybeFfiHelper txBody
      bignumToCoin = (pure <<< Coin) <=< bigNumToBigInt
      withdrawals = map M.fromFoldable <<< traverse (traverse bignumToCoin) =<< ws
  pure $ TxBody
    { inputs
    , outputs
    , fee
    , ttl: map Slot <<< UInt.fromNumber' =<< _txBodyTtl maybeFfiHelper txBody
    , certs: (map <<< map) convertCert $ _txBodyCerts containerHelper maybeFfiHelper txBody -- :: Maybe (Array Certificate)
    , withdrawals
    , update: convertUpdate =<< _txBodyUpdate maybeFfiHelper txBody
    , auxiliaryDataHash: _txBodyAuxiliaryDataHash maybeFfiHelper txBody
    , validityStartInterval: map Slot <<< UInt.fromInt' =<< _txBodyValidityStartInterval maybeFfiHelper txBody
    , mint: map convertMint $ _txBodyMint maybeFfiHelper txBody
    , scriptDataHash: _txBodyScriptDataHash maybeFfiHelper txBody
    , collateral: traverse convertInput =<< _txBodyCollateral containerHelper maybeFfiHelper txBody
    , requiredSigners: (map <<< map ) convertRequiredSigner $ _txBodyRequiredSigners maybeFfiHelper txBody
    , networkId
    }

convertRequiredSigner :: Ed25519KeyHash -> RequiredSigner
convertRequiredSigner = notImplemented -- TODO: how?

convertMint :: CSL.Mint -> Mint
convertMint = notImplemented

convertUpdate :: CSL.Update -> Maybe Update
convertUpdate = _unpackUpdate Tuple >>> \ { epoch, paramUpdates} -> do
  epoch' <- Epoch <$> UInt.fromInt' epoch
  let convertUpdates = ProposedProtocolParameterUpdates <<< M.fromFoldable <<< map (map convertProtocolParamUpdate)
  pure {epoch: epoch', proposedProtocolParameterUpdates: convertUpdates paramUpdates}

convertProtocolParamUpdate :: CSL.ProtocolParamUpdate -> ProtocolParamUpdate
convertProtocolParamUpdate = notImplemented

convertAuxiliaryData :: CSL.AuxiliaryData -> AuxiliaryData
convertAuxiliaryData ad =
  AuxiliaryData
  { metadata: convertGeneralTransactionMetadata <$> _adGeneralMetadata maybeFfiHelper ad
  , nativeScripts: convertNativeScripts =<< _adNativeScripts maybeFfiHelper ad
  , plutusScripts: convertPlutusScripts <$> _adPlutusScripts maybeFfiHelper ad
  }

convertGeneralTransactionMetadata :: CSL.GeneralTransactionMetadata -> GeneralTransactionMetadata
convertGeneralTransactionMetadata = notImplemented -- TODO

convertCert :: CSL.Certificate -> Certificate
convertCert = notImplemented -- TODO

foreign import _txBody :: CSL.Transaction -> CSL.TransactionBody
foreign import _txIsValid :: CSL.Transaction -> Boolean
foreign import _txWitnessSet :: CSL.Transaction -> CSL.TransactionWitnessSet
foreign import _txAuxiliaryData :: MaybeFfiHelper -> CSL.Transaction -> Maybe CSL.AuxiliaryData


foreign import _adGeneralMetadata :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe CSL.GeneralTransactionMetadata
foreign import _adNativeScripts :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe NativeScripts
foreign import _adPlutusScripts :: MaybeFfiHelper -> CSL.AuxiliaryData -> Maybe PlutusScripts

-- inputs(): TransactionInputs;
foreign import _txBodyInputs :: ContainerHelper -> CSL.TransactionBody -> Array CSL.TransactionInput
-- outputs(): TransactionOutputs;
foreign import _txBodyOutputs :: ContainerHelper -> CSL.TransactionBody -> Array CSL.TransactionOutput
-- fee(): BigNum;
foreign import _txBodyFee :: CSL.TransactionBody -> CSL.BigNum
-- ttl(): number | void;
foreign import _txBodyTtl :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Number
-- certs(): Certificates | void;
foreign import _txBodyCerts :: ContainerHelper -> MaybeFfiHelper -> CSL.TransactionBody -> Maybe (Array CSL.Certificate)
-- withdrawals(): Withdrawals | void
foreign import _txBodyWithdrawals :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Withdrawals
-- update(): Update | void
foreign import _txBodyUpdate :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Update
-- auxiliary_data_hash(): AuxiliaryDataHash | void
foreign import _txBodyAuxiliaryDataHash :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe AuxiliaryDataHash
-- validity_start_interval(): number | void
foreign import _txBodyValidityStartInterval :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Int
-- mint(): Mint | void
foreign import _txBodyMint :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Mint
-- multiassets(): Mint | void
foreign import _txBodyMultiAssets :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe CSL.Mint
-- script_data_hash(): ScriptDataHash | void
foreign import _txBodyScriptDataHash :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe ScriptDataHash
-- collateral(): TransactionInputs | void
foreign import _txBodyCollateral :: ContainerHelper -> MaybeFfiHelper -> CSL.TransactionBody -> Maybe (Array CSL.TransactionInput)
-- required_signers(): Ed25519KeyHashes | void
foreign import _txBodyRequiredSigners :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe (Array Ed25519KeyHash)
-- network_id(): NetworkId | void
foreign import _txBodyNetworkId :: MaybeFfiHelper -> CSL.TransactionBody -> Maybe Int

foreign import _unpackWithdrawals :: (forall a b. a -> b -> Tuple a b) -> CSL.Withdrawals -> Array (Tuple RewardAddress CSL.BigNum)

foreign import _unpackUpdate :: (forall a b. a -> b -> Tuple a b) -> CSL.Update -> { epoch :: Int, paramUpdates :: Array (Tuple GenesisHash CSL.ProtocolParamUpdate)}
