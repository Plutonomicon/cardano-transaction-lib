module Deserialization.Transaction
  ( CertConvHelper
  , MetadatumHelper
  , _adGeneralMetadata
  , _adNativeScripts
  , _adPlutusScripts
  , _convertCert
  , _convertLanguage
  , _convertMetadatum
  , _convertNonce
  , _txAuxiliaryData
  , _txBody
  , _txBodyAuxiliaryDataHash
  , _txBodyCerts
  , _txBodyCollateral
  , _txBodyFee
  , _txBodyInputs
  , _txBodyMultiAssets
  , _txBodyNetworkId
  , _txBodyOutputs
  , _txBodyRequiredSigners
  , _txBodyScriptDataHash
  , _txBodyTtl
  , _txBodyUpdate
  , _txBodyValidityStartInterval
  , _txBodyWithdrawals
  , _txIsValid
  , _txWitnessSet
  , _unpackCostModel
  , _unpackCostModels
  , _unpackExUnits
  , _unpackExUnitsPrices
  , _unpackMetadataList
  , _unpackMetadataMap
  , _unpackMetadatums
  , _unpackMint
  , _unpackMintAssets
  , _unpackProtocolParamUpdate
  , _unpackProtocolVersions
  , _unpackUnitInterval
  , _unpackUpdate
  , _unpackWithdrawals
  , convertAuxiliaryData
  , convertCertificate
  , convertCostModel
  , convertCostModels
  , convertExUnitPrices
  , convertExUnits
  , convertGeneralTransactionMetadata
  , convertLanguage
  , convertMetadataList
  , convertMetadataMap
  , convertMetadatum
  , convertMint
  , convertNonce
  , convertProtocolParamUpdate
  , convertProtocolVersions
  , convertTransaction
  , convertTxBody
  , convertUpdate
  , cslNumberToUInt
  , cslRatioToRational
  , deserializeTransaction
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Bitraversable (bitraverse)
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.Ratio (Ratio, reduce)
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple)
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Variant (Variant, inj)
import Deserialization.BigNum (bigNumToBigInt')
import Deserialization.Error
  ( Err
  , FromCslRepError
  , _fromCslRepError
  , addErrTrace
  , cslErr
  , fromCslRepError
  )
import Deserialization.FromBytes (fromBytes')
import Deserialization.UnspentOutput (convertInput, convertOutput)
import Deserialization.WitnessSet
  ( convertNativeScripts
  , convertPlutusScripts
  , convertWitnessSet
  )
import Error (E, notImplementedError)
import FfiHelpers
  ( ContainerHelper
  , ErrorFfiHelper
  , MaybeFfiHelper
  , containerHelper
  , errorHelper
  , maybeFfiHelper
  )
import Serialization (toBytes)
import Serialization.Address
  ( RewardAddress
  , intToNetworkId
  , Slot(Slot)
  , StakeCredential
  )
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Serialization.Types (NativeScripts, PlutusScripts)
import Serialization.Types as Csl
import Type.Row (type (+))
import Types.ByteArray (ByteArray)
import Types.CborBytes (CborBytes)
import Types.Int as Int
import Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  , Certificate(StakeDeregistration, StakeRegistration, StakeDelegation)
  , CostModel(CostModel)
  , Costmdls(Costmdls)
  , Epoch(Epoch)
  , ExUnitPrices
  , ExUnits
  , GenesisHash
  , Language(PlutusV1)
  , Mint(Mint)
  , Nonce(HashNonce, IdentityNonce)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , TxBody(TxBody)
  , Update
  )
import Types.Transaction as T
import Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatum(MetadataList, MetadataMap, Bytes, Int, Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Types.TokenName (TokenName, tokenNameFromAssetName)
import Cardano.Types.Value
  ( Coin(Coin)
  , NonAdaAsset(NonAdaAsset)
  , scriptHashAsCurrencySymbol
  )
import Untagged.Union (asOneOf)

-- | Deserializes CBOR encoded transaction to a CTL's native type.
deserializeTransaction :: forall (r :: Row Type). CborBytes -> Err r T.Transaction
deserializeTransaction txCbor = fromBytes' (unwrap txCbor) >>= convertTransaction

-- | Converts transaction from foreign CSL representation to CTL's one.
convertTransaction
  :: forall (r :: Row Type). Csl.Transaction -> Err r T.Transaction
convertTransaction tx = addErrTrace "convertTransaction" do
  witnessSet <- cslErr "convertWitnessSet" $ convertWitnessSet
    (_txWitnessSet tx)
  body <- convertTxBody $ _txBody tx
  auxiliaryData <- traverse convertAuxiliaryData
    (_txAuxiliaryData maybeFfiHelper tx)
  pure $ T.Transaction
    { body
    , witnessSet
    , isValid: _txIsValid tx
    , auxiliaryData
    }

-- | Converts transaction body from foreign CSL representation to CTL's one.
convertTxBody :: forall (r :: Row Type). Csl.TransactionBody -> Err r TxBody
convertTxBody txBody = do
  inputs <-
    _txBodyInputs containerHelper txBody
      # traverse (convertInput >>> cslErr "TransactionInput")
  outputs <-
    _txBodyOutputs containerHelper txBody
      # traverse (convertOutput >>> cslErr "TransactionOutput")
  fee <-
    Coin <$> (_txBodyFee txBody # bigNumToBigInt' "Tx fee")
  networkId <-
    _txBodyNetworkId maybeFfiHelper txBody
      # traverse (intToNetworkId >>> cslErr "NetworkId")
  let
    ws :: Maybe (Array (Tuple RewardAddress Csl.BigNum))
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

  certs <- addErrTrace "Tx body certificates"
    $ traverse (traverse convertCertificate)
    $ _txBodyCerts containerHelper
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
    , mint: map convertMint $ _txBodyMultiAssets maybeFfiHelper txBody
    , scriptDataHash: convertScriptDataHash <$> _txBodyScriptDataHash
        maybeFfiHelper
        txBody
    , collateral: _txBodyCollateral containerHelper maybeFfiHelper txBody >>=
        traverse convertInput
    , requiredSigners: _txBodyRequiredSigners maybeFfiHelper txBody #
        (map <<< map) RequiredSigner
    , networkId
    }

  where
  intToSlot
    :: forall (s :: Row Type)
     . Int
    -> Either (Variant (fromCslRepError :: String | s)) Slot
  intToSlot x =
    cslErr ("validityStartInterval UInt.fromInt': " <> show x)
      <<< map Slot
      <<< UInt.fromInt' $ x

convertUpdate :: forall (r :: Row Type). Csl.Update -> Err r Update
convertUpdate u = do
  let { epoch: e, paramUpdates } = _unpackUpdate containerHelper u
  epoch <- map Epoch $ cslNumberToUInt "convertUpdate: epoch" e
  ppus <- traverse (traverse convertProtocolParamUpdate) paramUpdates
  pure
    { epoch
    , proposedProtocolParameterUpdates: ProposedProtocolParameterUpdates $
        M.fromFoldable ppus
    }

-- | TODO partially implemented
convertCertificate
  :: forall (r :: Row Type). Csl.Certificate -> Err r Certificate
convertCertificate = _convertCert certConvHelper
  where
  certConvHelper :: CertConvHelper r
  certConvHelper =
    { stakeDeregistration: pure <<< StakeDeregistration
    , stakeRegistration: pure <<< StakeRegistration
    , stakeDelegation: \sc -> pure <<< StakeDelegation sc
    , notImplementedError: notImplementedError
    }

convertMint :: Csl.Mint -> Mint
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
  convAssetName :: Tuple Csl.AssetName Int -> Tuple TokenName BigInt
  convAssetName = bimap tokenNameFromAssetName BigInt.fromInt

convertProtocolParamUpdate
  :: forall (r :: Row Type)
   . Csl.ProtocolParamUpdate
  -> Err r ProtocolParamUpdate
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
  protocolVersion <- traverse (convertProtocolVersions (lbl "protocolVersion"))
    ppu.protocolVersion
  costModels <- traverse (convertCostModels (lbl "costModels")) ppu.costModels
  maxTxExUnits <- traverse (convertExUnits (lbl "maxTxExUnits"))
    ppu.maxTxExUnits
  maxBlockExUnits <- traverse (convertExUnits (lbl "maxBlockExUnits"))
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
    , poolPledgeInfluence: _unpackUnitInterval <$> ppu.poolPledgeInfluence
    , expansionRate: _unpackUnitInterval <$> ppu.expansionRate
    , treasuryGrowthRate: _unpackUnitInterval <$> ppu.treasuryGrowthRate
    , d: _unpackUnitInterval <$> ppu.d
    , extraEntropy: convertNonce <$> ppu.extraEntropy
    , protocolVersion
    , minPoolCost: ppu.minPoolCost
    , adaPerUtxoByte: ppu.adaPerUtxoByte
    , costModels
    , executionCosts: convertExUnitPrices <$> ppu.executionCosts
    , maxTxExUnits
    , maxBlockExUnits
    , maxValueSize
    }

convertNonce :: Csl.Nonce -> Nonce
convertNonce = _convertNonce
  { hashNonce: HashNonce, identityNonce: IdentityNonce }

convertCostModels
  :: forall (r :: Row Type)
   . String
  -> Csl.Costmdls
  -> E (FromCslRepError + r) Costmdls
convertCostModels nm cslCostMdls =
  let
    mdls :: Array (Tuple Csl.Language Csl.CostModel)
    mdls = _unpackCostModels containerHelper cslCostMdls
  in
    (Costmdls <<< M.fromFoldable) <$> traverse
      (bitraverse (convertLanguage nm) (convertCostModel nm))
      mdls

convertLanguage
  :: forall (r :: Row Type)
   . String
  -> Csl.Language
  -> E (FromCslRepError + r) Language
convertLanguage err = _convertLanguage
  (errorHelper (inj _fromCslRepError <<< \e -> err <> ": " <> e))
  { plutusV1: PlutusV1 }

convertCostModel
  :: forall (r :: Row Type)
   . String
  -> Csl.CostModel
  -> E (FromCslRepError + r) CostModel
convertCostModel err = map CostModel <<< traverse stringToUInt <<<
  _unpackCostModel
  where
  stringToUInt
    :: String -> Either (Variant (fromCslRepError :: String | r)) UInt
  stringToUInt s = cslErr (err <> ": string (" <> s <> ") -> uint") $
    UInt.fromString s

convertAuxiliaryData
  :: forall (r :: Row Type). Csl.AuxiliaryData -> Err r AuxiliaryData
convertAuxiliaryData ad = addErrTrace "convertAuxiliaryData" do
  metadata <- traverse convertGeneralTransactionMetadata
    (_adGeneralMetadata maybeFfiHelper ad)
  pure $ AuxiliaryData
    { metadata
    , nativeScripts: convertNativeScripts =<< _adNativeScripts maybeFfiHelper ad
    , plutusScripts: convertPlutusScripts <$> _adPlutusScripts maybeFfiHelper ad
    }

convertGeneralTransactionMetadata
  :: forall (r :: Row Type)
   . Csl.GeneralTransactionMetadata
  -> Err r GeneralTransactionMetadata
convertGeneralTransactionMetadata =
  -- unpack to array of tuples
  _unpackMetadatums containerHelper
    >>>
      -- convert tuple type
      traverse
        ( bitraverse
            ( map TransactionMetadatumLabel <<< bigNumToBigInt'
                "MetadatumLabel: "
            )
            (convertMetadatum "GeneralTransactionMetadata: ")
        )
    -- fold to map and and wrap
    >>> map (M.fromFoldable >>> wrap)

convertMetadatum
  :: forall (r :: Row Type)
   . String
  -> Csl.TransactionMetadatum
  -> Err r TransactionMetadatum
convertMetadatum err = fix \_ -> addErrTrace err <<< _convertMetadatum
  ( { error: fromCslRepError
    , from_bytes: pure <<< Bytes
    , from_int: map Int <<< stringToInt "Metadatum Int"
    , from_text: pure <<< Text
    , from_map: convertMetadataMap convertMetadatum
    , from_list: convertMetadataList convertMetadatum
    }
  )

  where
  stringToInt
    :: forall (s :: Row Type)
     . String
    -> String
    -> Either (Variant (fromCslRepError :: String | s)) Int.Int
  stringToInt s = cslErr (err <> ": string (" <> s <> ") -> bigint") <<<
    (Int.fromBigInt <=< BigInt.fromString)

convertMetadataList
  :: forall (r :: Row Type)
   . (String -> Csl.TransactionMetadatum -> Err r TransactionMetadatum)
  -> Csl.MetadataList
  -> Err r TransactionMetadatum
convertMetadataList convert = map MetadataList
  <<< traverse (convert "convertMetadataList")
  <<< _unpackMetadataList containerHelper

convertMetadataMap
  :: forall (r :: Row Type)
   . (String -> Csl.TransactionMetadatum -> Err r TransactionMetadatum)
  -> Csl.MetadataMap
  -> Err r TransactionMetadatum
convertMetadataMap convert =
  -- unpack to array of tuples
  _unpackMetadataMap containerHelper
    >>>
      -- convert tuple type
      traverse
        ( bitraverse
            (convert "convertMetadataMap key")
            (convert "convertMetadataMap value")
        )
    -- fold to map and and wrap
    >>> map (M.fromFoldable >>> MetadataMap)

---- conversion helpers

cslNumberToUInt
  :: forall (r :: Row Type). String -> Number -> E (FromCslRepError + r) UInt
cslNumberToUInt nm nb = cslErr (nm <> ": Number (" <> show nb <> ") -> UInt") $
  UInt.fromNumber' nb

cslRatioToRational
  :: forall (r :: Row Type)
   . String
  -> { denominator :: Csl.BigNum, numerator :: Csl.BigNum }
  -> E (FromCslRepError + r) (Ratio BigInt)
cslRatioToRational err { numerator, denominator } = reduce
  <$> bigNumToBigInt' (err <> " cslRatioToRational") numerator
  <*> bigNumToBigInt' (err <> " cslRatioToRational") denominator

convertExUnits
  :: forall (r :: Row Type)
   . String
  -> Csl.ExUnits
  -> E (FromCslRepError + r) ExUnits
convertExUnits nm cslExunits =
  let
    { mem, steps } = _unpackExUnits cslExunits
  in
    { mem: _, steps: _ }
      <$> bigNumToBigInt' (nm <> " mem") mem
      <*> bigNumToBigInt' (nm <> " steps") steps

convertScriptDataHash :: Csl.ScriptDataHash -> ScriptDataHash
convertScriptDataHash = asOneOf >>> toBytes >>> ScriptDataHash

convertProtocolVersions
  :: forall (r :: Row Type)
   . String
  -> Csl.ProtocolVersions
  -> E (FromCslRepError + r) (Array ProtocolVersion)
convertProtocolVersions nm cslPV =
  for (_unpackProtocolVersions containerHelper cslPV)
    ( \{ major, minor } ->
        { major: _, minor: _ }
          <$> cslNumberToUInt (nm <> " major") major
          <*> cslNumberToUInt (nm <> " minor") minor
    )

---- foreign imports

foreign import _convertNonce
  :: { identityNonce :: Nonce, hashNonce :: ByteArray -> Nonce }
  -> Csl.Nonce
  -> Nonce

foreign import _unpackProtocolParamUpdate
  :: MaybeFfiHelper
  -> Csl.ProtocolParamUpdate
  -> { minfeeA :: Maybe Csl.BigNum
     , minfeeB :: Maybe Csl.BigNum
     , maxBlockBodySize :: Maybe Number
     , maxTxSize :: Maybe Number
     , maxBlockHeaderSize :: Maybe Number
     , keyDeposit :: Maybe Csl.BigNum
     , poolDeposit :: Maybe Csl.BigNum
     , maxEpoch :: Maybe Number
     , nOpt :: Maybe Number
     , poolPledgeInfluence :: Maybe Csl.UnitInterval
     , expansionRate ::
         Maybe Csl.UnitInterval
     , treasuryGrowthRate ::
         Maybe Csl.UnitInterval
     , d :: Maybe Csl.UnitInterval
     , extraEntropy :: Maybe Csl.Nonce
     , protocolVersion :: Maybe Csl.ProtocolVersions
     , minPoolCost :: Maybe Csl.BigNum
     , adaPerUtxoByte :: Maybe Csl.BigNum
     , costModels :: Maybe Csl.Costmdls
     , executionCosts :: Maybe Csl.ExUnitPrices
     , maxTxExUnits :: Maybe Csl.ExUnits
     , maxBlockExUnits :: Maybe Csl.ExUnits
     , maxValueSize :: Maybe Number
     }

foreign import _unpackCostModels
  :: ContainerHelper -> Csl.Costmdls -> Array (Tuple Csl.Language Csl.CostModel)

foreign import _unpackCostModel :: Csl.CostModel -> Array String

foreign import _unpackMetadataMap
  :: ContainerHelper
  -> Csl.MetadataMap
  -> Array (Tuple Csl.TransactionMetadatum Csl.TransactionMetadatum)

foreign import _unpackMetadataList
  :: ContainerHelper -> Csl.MetadataList -> Array Csl.TransactionMetadatum

type MetadatumHelper (r :: Row Type) =
  { from_map :: Csl.MetadataMap -> Err r TransactionMetadatum
  , from_list :: Csl.MetadataList -> Err r TransactionMetadatum
  , from_int :: String -> Err r TransactionMetadatum
  , from_text :: String -> Err r TransactionMetadatum
  , from_bytes :: ByteArray -> Err r TransactionMetadatum
  , error :: String -> Err r TransactionMetadatum
  }

foreign import _unpackProtocolVersions
  :: ContainerHelper
  -> Csl.ProtocolVersions
  -> Array { major :: Number, minor :: Number }

foreign import _unpackExUnits
  :: Csl.ExUnits -> { mem :: Csl.BigNum, steps :: Csl.BigNum }

foreign import _unpackUnitInterval
  :: Csl.UnitInterval -> { numerator :: Csl.BigNum, denominator :: Csl.BigNum }

convertExUnitPrices :: Csl.ExUnitPrices -> ExUnitPrices
convertExUnitPrices cslEUP =
  let
    { memPrice, stepPrice } = _unpackExUnitsPrices cslEUP
  in
    { memPrice: _unpackUnitInterval memPrice
    , stepPrice: _unpackUnitInterval stepPrice
    }

foreign import _unpackExUnitsPrices
  :: Csl.ExUnitPrices
  -> { memPrice :: Csl.UnitInterval, stepPrice :: Csl.UnitInterval }

foreign import _convertMetadatum
  :: forall (r :: Row Type)
   . MetadatumHelper r
  -> Csl.TransactionMetadatum
  -> E (FromCslRepError + r) TransactionMetadatum

foreign import _unpackMetadatums
  :: ContainerHelper
  -> Csl.GeneralTransactionMetadata
  -> Array (Tuple Csl.BigNum Csl.TransactionMetadatum)

foreign import _txBody :: Csl.Transaction -> Csl.TransactionBody
foreign import _txIsValid :: Csl.Transaction -> Boolean
foreign import _txWitnessSet :: Csl.Transaction -> Csl.TransactionWitnessSet
foreign import _txAuxiliaryData
  :: MaybeFfiHelper -> Csl.Transaction -> Maybe Csl.AuxiliaryData

foreign import _adGeneralMetadata
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe Csl.GeneralTransactionMetadata

foreign import _adNativeScripts
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe NativeScripts

foreign import _adPlutusScripts
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe PlutusScripts

-- inputs(): TransactionInputs;
foreign import _txBodyInputs
  :: ContainerHelper -> Csl.TransactionBody -> Array Csl.TransactionInput

-- outputs(): TransactionOutputs;
foreign import _txBodyOutputs
  :: ContainerHelper -> Csl.TransactionBody -> Array Csl.TransactionOutput

-- fee(): BigNum;
foreign import _txBodyFee :: Csl.TransactionBody -> Csl.BigNum
-- ttl(): number | void;
foreign import _txBodyTtl
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Number

-- certs(): Certificates | void;
foreign import _txBodyCerts
  :: ContainerHelper
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe (Array Csl.Certificate)

-- withdrawals(): Withdrawals | void
foreign import _txBodyWithdrawals
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.Withdrawals

-- update(): Update | void
foreign import _txBodyUpdate
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.Update

-- auxiliary_data_hash(): AuxiliaryDataHash | void
foreign import _txBodyAuxiliaryDataHash
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe AuxiliaryDataHash

-- validity_start_interval(): number | void
foreign import _txBodyValidityStartInterval
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Int

-- multiassets(): Mint | void
foreign import _txBodyMultiAssets
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.Mint

-- script_data_hash(): ScriptDataHash | void
foreign import _txBodyScriptDataHash
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.ScriptDataHash

-- collateral(): TransactionInputs | void
foreign import _txBodyCollateral
  :: ContainerHelper
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe (Array Csl.TransactionInput)

-- required_signers(): Ed25519KeyHashes | void
foreign import _txBodyRequiredSigners
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe (Array Ed25519KeyHash)

-- network_id(): NetworkId | void
foreign import _txBodyNetworkId
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Int

foreign import _unpackWithdrawals
  :: ContainerHelper
  -> Csl.Withdrawals
  -> Array (Tuple RewardAddress Csl.BigNum)

foreign import _unpackUpdate
  :: ContainerHelper
  -> Csl.Update
  -> { epoch :: Number
     , paramUpdates :: Array (Tuple GenesisHash Csl.ProtocolParamUpdate)
     }

foreign import _unpackMint
  :: ContainerHelper -> Csl.Mint -> Array (Tuple ScriptHash Csl.MintAssets)

foreign import _unpackMintAssets
  :: ContainerHelper -> Csl.MintAssets -> Array (Tuple Csl.AssetName Int)

type CertConvHelper (r :: Row Type) =
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

foreign import _convertCert
  :: forall (r :: Row Type)
   . CertConvHelper r
  -> Csl.Certificate
  -> Err r Certificate

foreign import _convertLanguage
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> { plutusV1 :: Language }
  -> Csl.Language
  -> E r Language
