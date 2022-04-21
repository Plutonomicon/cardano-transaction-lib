module Deserialization.Transaction where

import Prelude

import Contract.Address (RequiredSigner(..), Slot(..), StakeCredential)
import Contract.Numeric.Rational (reduce)
import Contract.Prelude (Tuple, traverse)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (Ed25519KeyHash)
import Contract.Transaction
  ( AuxiliaryData(..)
  , AuxiliaryDataHash
  , Certificate(..)
  , CostModel(..)
  , Costmdls(..)
  , Epoch(..)
  , ExUnits
  , GeneralTransactionMetadata
  , GenesisHash
  , Language(..)
  , Mint(..)
  , Nonce(..)
  , ProposedProtocolParameterUpdates(..)
  , ProtocolParamUpdate
  , ProtocolVersion
  , ScriptDataHash
  , TxBody(..)
  , Update
  )
import Contract.Value (Coin(..), NonAdaAsset(..), TokenName)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Bitraversable (bitraverse)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Ratio (Ratio)
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Variant (inj)
import Deserialization.BigNum (bigNumToBigInt')
import Deserialization.Error
  ( FromCslRepError
  , _fromCslRepError
  , fromCslRepError
  )
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
  , ErrorFfiHelper
  , MaybeFfiHelper
  , containerHelper
  , errorHelper
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

noteErr
  :: forall a r
   . String
  -> Maybe a
  -> E (FromCslRepError + r) a
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
    Coin <$> (_txBodyFee txBody # bigNumToBigInt' "txBody fee")
  networkId <-
    _txBodyNetworkId maybeFfiHelper txBody
      # traverse (intToNetworkId >>> noteErr "NetworkId")
  let
    ws :: Maybe (Array (Tuple RewardAddress CSL.BigNum))
    ws = _unpackWithdrawals containerHelper <$> _txBodyWithdrawals
      maybeFfiHelper
      txBody

  withdrawals :: Maybe (M.Map RewardAddress Coin) <-
    -- array -> map
    (map <<< map) M.fromFoldable
      -- bignum -> coin
      <<< (traverse <<< traverse <<< traverse)
        (bigNumToBigInt' "txbody withdrawals" >>> map Coin)
      $ ws

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
        (map <<< map) RequiredSigner
    , networkId
    }

  where
  intToSlot x =
    noteErr ("validityStartInterval UInt.fromInt': " <> show x)
      <<< map Slot
      <<< UInt.fromInt' $ x

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
convertUpdate u = do
  let { epoch: e, paramUpdates } = _unpackUpdate containerHelper u
  epoch <- map Epoch $ cslNumberToUInt "convertUpdate: epoch" e
  ppus <- traverse (traverse convertProtocolParamUpdate) paramUpdates
  pure
    { epoch
    , proposedProtocolParameterUpdates: ProposedProtocolParameterUpdates $
        M.fromFoldable ppus
    }

convertProtocolParamUpdate
  :: forall r. CSL.ProtocolParamUpdate -> Err r ProtocolParamUpdate
convertProtocolParamUpdate cslPpu = do
  let
    ppu = _unpackProtocolParamUpdate maybeFfiHelper cslPpu
    lbl = (<>) "ProtocolParamUpdate."

  minfeeA <- traverse (map Coin <<< bigNumToBigInt' (lbl "minfeeA")) ppu.minfeeA
  minfeeB <- traverse (map Coin <<< bigNumToBigInt' (lbl "minfeeB")) ppu.minfeeB
  maxBlockBodySize <- traverse (cslNumberToUInt (lbl "maxBlockBodySize"))
    ppu.maxBlockBodySize
  maxTxSize <- traverse (cslNumberToUInt (lbl "maxTxSize")) ppu.maxTxSize
  maxBlockHeaderSize <- traverse (cslNumberToUInt (lbl "maxBlockHeaderSize"))
    ppu.maxBlockHeaderSize
  keyDeposit <- traverse (map Coin <<< bigNumToBigInt' (lbl "keyDeposit"))
    ppu.keyDeposit
  poolDeposit <- traverse (map Coin <<< bigNumToBigInt' (lbl "poolDeposit"))
    ppu.poolDeposit
  maxEpoch <- traverse (map Epoch <<< cslNumberToUInt (lbl "maxEpoch"))
    ppu.maxEpoch
  nOpt <- traverse (cslNumberToUInt (lbl "nOpt")) ppu.nOpt
  poolPledgeInfluence <- traverse
    (cslRatioToRational (lbl "poolPledgeInfluence"))
    ppu.poolPledgeInfluence
  protocolVersion <- traverse
    (traverse (cslToProtocolVersion (lbl "protocolVersion")))
    ppu.protocolVersion
  minPoolCost <- traverse (map Coin <<< bigNumToBigInt' (lbl "minPoolCost"))
    ppu.minPoolCost
  adaPerUtxoByte <- traverse
    (map Coin <<< bigNumToBigInt' (lbl "adaPerUtxoByte"))
    ppu.adaPerUtxoByte
  costModels <- traverse (convertCostModels (lbl "costModels")) ppu.costModels
  maxTxExUnits <- traverse (cslToExunits (lbl "maxTxExUnits")) ppu.maxTxExUnits
  maxBlockExUnits <- traverse (cslToExunits (lbl "maxBlockExUnits"))
    ppu.maxBlockExUnits
  maxValueSize <- traverse (cslNumberToUInt (lbl "maxValueSize"))
    ppu.maxValueSize
  pure
    { minfeeA
    , minfeeB
    , maxBlockBodySize
    , maxTxSize
    , maxBlockHeaderSize
    , keyDeposit
    , poolDeposit
    , maxEpoch
    , nOpt
    , poolPledgeInfluence
    , expansionRate: ppu.expansionRate
    , treasuryGrowthRate: ppu.treasuryGrowthRate
    , d: ppu.d
    , extraEntropy: convertNonce <$> ppu.extraEntropy
    , protocolVersion
    , minPoolCost
    , adaPerUtxoByte
    , costModels
    , executionCosts: ppu.executionCosts
    , maxTxExUnits
    , maxBlockExUnits
    , maxValueSize
    }

-- where

-- declare export class ProtocolParamUpdate {
--   minfee_a(): Maybe BigNum
--   minfee_b(): Maybe BigNum
--   max_tx_size(): Maybe Number
--   max_block_header_size(): Maybe Number
--   key_deposit(): Maybe BigNum
--   pool_deposit(): Maybe BigNum
--   max_epoch(): Maybe Number
--   n_opt(): Maybe Number
--   pool_pledge_influence(): Maybe { numerator :: BigNum, denominator :: BigNum }
--   expansion_rate(): Maybe   { numerator :: BigNum, denominator :: BigNum  }
--   treasury_growth_rate(): Maybe { numerator :: BigNum, denominator :: BigNum }
--   d(): Maybe   { numerator :: BigNum, denominator :: BigNum  }
--   extra_entropy(): Maybe Nonce
--   protocol_version(): Maybe (Array {major :: Number, minor :: minor})
--   min_pool_cost(): Maybe BigNum
--   ada_per_utxo_byte(): Maybe BigNum
--   cost_models(): Maybe CSL.Costmdls
--   execution_costs(): Maybe { memPrice :: { numerator :: BigNum, denominator :: BigNum }, stepPrice :: { numerator :: BigNum, denominator :: BigNum }}
--   max_tx_ex_units(): Maybe { mem :: BigNum, steps :: BigNum }
--   max_block_ex_units(): Maybe { mem :: BigNum, steps :: BigNum }
--   max_value_size(): Maybe Number
--   collateral_percentage(): Maybe Number
--   max_collateral_inputs(): Maybe Number
-- }

foreign import _unpackProtocolParamUpdate
  :: MaybeFfiHelper
  -> CSL.ProtocolParamUpdate
  -> { minfeeA :: Maybe CSL.BigNum
     , minfeeB :: Maybe CSL.BigNum
     , maxBlockBodySize :: Maybe Number
     , maxTxSize :: Maybe Number
     , maxBlockHeaderSize :: Maybe Number
     , keyDeposit :: Maybe CSL.BigNum
     , poolDeposit :: Maybe CSL.BigNum
     , maxEpoch :: Maybe Number
     , nOpt :: Maybe Number
     , poolPledgeInfluence ::
         Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
     , expansionRate ::
         Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
     , treasuryGrowthRate ::
         Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
     , d :: Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
     , extraEntropy :: Maybe CSL.Nonce
     , protocolVersion :: Maybe (Array { major :: Number, minor :: Number })
     , minPoolCost :: Maybe CSL.BigNum
     , adaPerUtxoByte :: Maybe CSL.BigNum
     , costModels :: Maybe CSL.Costmdls
     , executionCosts ::
         Maybe
           { memPrice :: { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
           , stepPrice :: { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
           }
     , maxTxExUnits :: Maybe { mem :: CSL.BigNum, steps :: CSL.BigNum }
     , maxBlockExUnits :: Maybe { mem :: CSL.BigNum, steps :: CSL.BigNum }
     , maxValueSize :: Maybe Number
     , collateralPercentage :: Maybe Number
     , maxCollateralInputs :: Maybe Number
     }

convertNonce :: CSL.Nonce -> Nonce
convertNonce = _convertNonce
  { hashNonce: HashNonce, identityNonce: IdentityNonce }

foreign import _convertNonce
  :: { identityNonce :: Nonce, hashNonce :: ByteArray -> Nonce }
  -> CSL.Nonce
  -> Nonce

convertCostModels
  :: forall (r :: Row Type)
   . String
  -> CSL.Costmdls
  -> E (FromCslRepError + r) Costmdls
convertCostModels nm cslCostMdls =
  let
    mdls :: Array (Tuple CSL.Language CSL.CostModel)
    mdls = _unpackCostModels containerHelper cslCostMdls
  in
    (Costmdls <<< M.fromFoldable) <$> traverse
      (bitraverse (convertLanguage nm) (convertCostModel nm))
      mdls

convertLanguage
  :: forall r. String -> CSL.Language -> E (FromCslRepError + r) Language
convertLanguage err = _convertLanguage
  (errorHelper (inj _fromCslRepError.proxy <<< \e -> err <> ": " <> e))
  { plutusV1: PlutusV1 }

foreign import _unpackCostModels
  :: ContainerHelper -> CSL.Costmdls -> Array (Tuple CSL.Language CSL.CostModel)

foreign import _unpackCostModel :: CSL.CostModel -> Array String

convertCostModel
  :: forall r. String -> CSL.CostModel -> E (FromCslRepError + r) CostModel
convertCostModel err = map CostModel <<< traverse stringToUInt <<<
  _unpackCostModel
  where
  stringToUInt s = noteErr (err <> ": string (" <> s <> ") -> uint") $
    UInt.fromString s

convertAuxiliaryData
  :: forall (r :: Row Type). CSL.AuxiliaryData -> AuxiliaryData
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
  -> { epoch :: Number
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
certConvHelper =
  { stakeDeregistration: pure <<< StakeDeregistration
  , stakeRegistration: pure <<< StakeRegistration
  , stakeDelegation: \sc -> pure <<< StakeDelegation sc
  , notImplementedError: notImplementedError
  }

foreign import _convertCert
  :: forall r. CertConvHelper r -> CSL.Certificate -> Err r Certificate

foreign import _convertLanguage
  :: forall r
   . ErrorFfiHelper r
  -> { plutusV1 :: Language }
  -> CSL.Language
  -> E r Language

cslNumberToUInt :: forall r. String -> Number -> E (FromCslRepError + r) UInt
cslNumberToUInt nm nb = noteErr (nm <> ": Number (" <> show nb <> ") -> UInt") $
  UInt.fromNumber' nb

cslRatioToRational
  :: forall r
   . String
  -> { denominator :: CSL.BigNum, numerator :: CSL.BigNum }
  -> E (FromCslRepError + r) (Ratio BigInt)
cslRatioToRational err { numerator, denominator } = reduce
  <$> bigNumToBigInt' (err <> " cslRatioToRational") numerator
  <*> bigNumToBigInt' (err <> " cslRatioToRational") denominator

cslToExunits
  :: forall r
   . String
  -> { mem :: CSL.BigNum, steps :: CSL.BigNum }
  -> E (FromCslRepError + r) ExUnits
cslToExunits nm { mem, steps } = { mem: _, steps: _ }
  <$> bigNumToBigInt' (nm <> " mem") mem
  <*> bigNumToBigInt' (nm <> " steps") steps

cslToProtocolVersion
  :: forall r
   . String
  -> { major :: Number, minor :: Number }
  -> E (FromCslRepError + r) ProtocolVersion
cslToProtocolVersion nm { major, minor } = { major: _, minor: _ }
  <$> cslNumberToUInt (nm <> " major") major
  <*> cslNumberToUInt (nm <> " minor") minor
