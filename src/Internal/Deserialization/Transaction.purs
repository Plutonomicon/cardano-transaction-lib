module Ctl.Internal.Deserialization.Transaction
  ( CertConvHelper
  , MetadatumHelper
  , _adGeneralMetadata
  , _adNativeScripts
  , _adPlutusScripts
  , _convertCert
  , _convertMetadatum
  , _convertNonce
  , _txAuxiliaryData
  , _txBody
  , _txBodyAuxiliaryDataHash
  , _txBodyCerts
  , _txBodyCollateral
  , _txBodyFee
  , _txBodyInputs
  , _txBodyMint
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
  , _unpackProtocolVersion
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
  , convertMetadataList
  , convertMetadataMap
  , convertMetadatum
  , convertMint
  , convertNonce
  , convertProtocolParamUpdate
  , convertProtocolVersion
  , convertTransaction
  , convertTxBody
  , convertUpdate
  , cslNumberToUInt
  , cslRatioToRational
  , deserializeTransaction
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash(AuxiliaryDataHash)
  , Certificate
      ( StakeDeregistration
      , StakeRegistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , GenesisKeyDelegation
      , MoveInstantaneousRewardsCert
      )
  , CostModel(CostModel)
  , Costmdls(Costmdls)
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
  , Nonce(HashNonce, IdentityNonce)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  , Relay(SingleHostAddr, SingleHostName, MultiHostName)
  , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , Transaction(Transaction)
  , TxBody(TxBody)
  , URL(URL)
  , Update
  ) as T
import Ctl.Internal.Cardano.Types.Transaction (PoolPubKeyHash(PoolPubKeyHash))
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , mkNonAdaAsset
  , scriptHashAsCurrencySymbol
  )
import Ctl.Internal.Cardano.Types.Value (CurrencySymbol) as Csl
import Ctl.Internal.Deserialization.Error
  ( Err
  , FromCslRepError
  , addErrTrace
  , cslErr
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes')
import Ctl.Internal.Deserialization.Language (convertLanguage)
import Ctl.Internal.Deserialization.UnspentOutput (convertInput, convertOutput)
import Ctl.Internal.Deserialization.WitnessSet
  ( convertNativeScripts
  , convertPlutusScripts
  , convertWitnessSet
  )
import Ctl.Internal.Error (E)
import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.Address
  ( NetworkId(TestnetId, MainnetId)
  , RewardAddress
  , StakeCredential
  ) as Csl
import Ctl.Internal.Serialization.Address (Slot(Slot))
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ScriptHash, VRFKeyHash) as Csl
import Ctl.Internal.Serialization.Types
  ( AssetName
  , AuxiliaryData
  , AuxiliaryDataHash
  , Certificate
  , CostModel
  , Costmdls
  , ExUnitPrices
  , ExUnits
  , GeneralTransactionMetadata
  , GenesisDelegateHash
  , GenesisHash
  , Ipv4
  , Ipv6
  , Language
  , MIRToStakeCredentials
  , MetadataList
  , MetadataMap
  , Mint
  , MintAssets
  , MintsAssets
  , MultiHostName
  , NativeScripts
  , Nonce
  , PlutusScripts
  , PoolMetadata
  , PoolMetadataHash
  , PoolParams
  , ProtocolParamUpdate
  , ProtocolVersion
  , Relay
  , ScriptDataHash
  , SingleHostAddr
  , SingleHostName
  , Transaction
  , TransactionBody
  , TransactionInput
  , TransactionMetadatum
  , TransactionOutput
  , TransactionWitnessSet
  , UnitInterval
  , Update
  , Withdrawals
  ) as Csl
import Ctl.Internal.Types.BigNum (BigNum) as Csl
import Ctl.Internal.Types.BigNum (toBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Int (Int) as Csl
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.RewardAddress (RewardAddress(RewardAddress)) as T
import Ctl.Internal.Types.TokenName (TokenName, tokenNameFromAssetName)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatum(MetadataList, MetadataMap, Bytes, Int, Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash(VRFKeyHash))
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Ratio (Ratio, reduce)
import Data.Set (fromFoldable) as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Variant (Variant)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Type.Row (type (+))

-- | Deserializes CBOR encoded transaction to a CTL's native type.
deserializeTransaction
  :: forall (r :: Row Type). CborBytes -> Err r T.Transaction
deserializeTransaction txCbor = fromBytes' (unwrap txCbor) >>=
  convertTransaction

-- | Converts transaction from foreign CSL representation to CTL's one.
convertTransaction
  :: forall (r :: Row Type). Csl.Transaction -> Err r T.Transaction
convertTransaction tx = addErrTrace "convertTransaction" do
  witnessSet <- cslErr "convertWitnessSet" $ convertWitnessSet
    (_txWitnessSet tx)
  body <- convertTxBody $ _txBody tx
  let
    auxiliaryData = convertAuxiliaryData <$>
      _txAuxiliaryData maybeFfiHelper tx
  pure $ T.Transaction
    { body
    , witnessSet
    , isValid: _txIsValid tx
    , auxiliaryData
    }

-- | Converts transaction body from foreign CSL representation to CTL's one.
convertTxBody :: forall (r :: Row Type). Csl.TransactionBody -> Err r T.TxBody
convertTxBody txBody = do
  let
    inputs = Set.fromFoldable $ convertInput <$> _txBodyInputs containerHelper
      txBody

  outputs <-
    _txBodyOutputs containerHelper txBody
      # traverse (convertOutput >>> cslErr "TransactionOutput")
  let
    fee = Coin $ BigNum.toBigInt $ _txBodyFee txBody
    networkId =
      _txBodyNetworkId Csl.TestnetId Csl.MainnetId maybeFfiHelper txBody

    ws :: Maybe (Array (Csl.RewardAddress /\ Csl.BigNum))
    ws = _unpackWithdrawals containerHelper <$> _txBodyWithdrawals
      maybeFfiHelper
      txBody

  withdrawals :: Maybe (M.Map T.RewardAddress Coin) <-
    -- array -> map
    (map <<< map) (M.fromFoldable <<< map (lmap T.RewardAddress))
      -- bignum -> coin
      <<< (traverse <<< traverse <<< traverse)
        (BigNum.toBigInt >>> Coin >>> pure)
      $ ws

  update <- traverse convertUpdate $ _txBodyUpdate maybeFfiHelper txBody

  let
    cslReferenceInputs :: Array Csl.TransactionInput
    cslReferenceInputs =
      _txBodyReferenceInputs maybeFfiHelper containerHelper txBody
        # fromMaybe mempty

    referenceInputs = Set.fromFoldable $ convertInput <$> cslReferenceInputs

    certs = map convertCertificate <$>
      _txBodyCerts containerHelper
        maybeFfiHelper
        txBody

  collateralReturn <-
    _txBodyCollateralReturn maybeFfiHelper txBody #
      traverse (convertOutput >>> cslErr "TransactionOutput")

  let
    totalCollateral = _txBodyTotalCollateral maybeFfiHelper txBody <#>
      (BigNum.toBigInt >>> Coin)

  pure $ T.TxBody
    { inputs
    , outputs
    , fee
    , ttl: Slot <$> _txBodyTtl maybeFfiHelper txBody
    , certs
    , withdrawals
    , update
    , auxiliaryDataHash:
        T.AuxiliaryDataHash <<< unwrap <<< toBytes <$>
          _txBodyAuxiliaryDataHash maybeFfiHelper txBody
    , validityStartInterval:
        Slot <$> _txBodyValidityStartInterval maybeFfiHelper txBody
    , mint: map convertMint $ _txBodyMint maybeFfiHelper txBody
    , referenceInputs
    , scriptDataHash: convertScriptDataHash <$> _txBodyScriptDataHash
        maybeFfiHelper
        txBody
    , collateral: _txBodyCollateral containerHelper maybeFfiHelper txBody >>=
        map convertInput >>> pure
    , requiredSigners:
        _txBodyRequiredSigners containerHelper maybeFfiHelper txBody #
          (map <<< map) T.RequiredSigner
    , networkId
    , collateralReturn
    , totalCollateral
    }

convertUpdate :: forall (r :: Row Type). Csl.Update -> Err r T.Update
convertUpdate u = do
  let { epoch: e, paramUpdates } = _unpackUpdate containerHelper u
  epoch <- map T.Epoch $ cslNumberToUInt "convertUpdate: epoch" e
  ppus <- traverse
    ( bitraverse
        (pure <<< T.GenesisHash <<< unwrap <<< toBytes)
        convertProtocolParamUpdate
    )
    paramUpdates
  pure
    { epoch
    , proposedProtocolParameterUpdates: T.ProposedProtocolParameterUpdates $
        M.fromFoldable ppus
    }

convertCertificate
  :: Csl.Certificate -> T.Certificate
convertCertificate = _convertCert certConvHelper
  where
  certConvHelper :: CertConvHelper T.Certificate
  certConvHelper =
    { stakeDeregistration: T.StakeDeregistration
    , stakeRegistration: T.StakeRegistration
    , stakeDelegation: \sc -> T.StakeDelegation sc <<< wrap <<< wrap
    , poolRegistration: convertPoolRegistration
    , poolRetirement: convertPoolRetirement
    , genesisKeyDelegation: \genesisHash genesisDelegateHash vrfKeyhash -> do
        T.GenesisKeyDelegation
          { genesisHash: T.GenesisHash $ unwrap $ toBytes genesisHash
          , genesisDelegateHash: T.GenesisDelegateHash $ unwrap
              $ toBytes genesisDelegateHash
          , vrfKeyhash: VRFKeyHash vrfKeyhash
          }
    , moveInstantaneousRewardsToOtherPotCert: \pot amount -> do
        T.MoveInstantaneousRewardsCert $
          T.ToOtherPot { pot, amount: amount }
    , moveInstantaneousRewardsToStakeCreds: \pot amounts -> do
        T.MoveInstantaneousRewardsCert $
          T.ToStakeCreds { pot, amounts: convertMIRToStakeCredentials amounts }
    }

convertMIRToStakeCredentials
  :: Csl.MIRToStakeCredentials -> T.MIRToStakeCredentials
convertMIRToStakeCredentials =
  T.MIRToStakeCredentials <<< M.fromFoldable <<< unpackMIRToStakeCredentials_
    containerHelper

convertPoolRegistration :: Csl.PoolParams -> T.Certificate
convertPoolRegistration params = do
  let
    relays = convertRelay <$> poolParamsRelays containerHelper params
  T.PoolRegistration
    { operator: PoolPubKeyHash $ wrap $ poolParamsOperator params
    , vrfKeyhash: VRFKeyHash $ poolParamsVrfKeyhash params
    , pledge: poolParamsPledge params
    , cost: poolParamsCost params
    , margin: _unpackUnitInterval $ poolParamsMargin params
    , rewardAccount: T.RewardAddress $ poolParamsRewardAccount params
    , poolOwners: wrap <<< wrap <$> poolParamsPoolOwners containerHelper params
    , relays
    , poolMetadata: poolParamsPoolMetadata maybeFfiHelper params <#>
        convertPoolMetadata_
          \url hash -> T.PoolMetadata
            { url: T.URL url
            , hash: T.PoolMetadataHash $ unwrap $ toBytes hash
            }
    }

type ConvertRelayHelper a =
  { asSingleHostAddr :: Csl.SingleHostAddr -> a
  , asSingleHostName :: Csl.SingleHostName -> a
  , asMultiHostName :: Csl.MultiHostName -> a
  }

convertRelay :: Csl.Relay -> T.Relay
convertRelay relay = do
  convertRelay_
    { asSingleHostAddr: convertSingleHostAddr_ maybeFfiHelper
        \mbPort mbIpv4 mbIpv6 -> do
          let
            ipv4 = mbIpv4 <#> convertIpv4
            ipv6 = mbIpv6 <#> convertIpv6
          T.SingleHostAddr { port: mbPort, ipv4, ipv6 }
    , asSingleHostName: convertSingleHostName_ maybeFfiHelper
        \port mbHost -> T.SingleHostName { port, dnsName: mbHost }
    , asMultiHostName: T.MultiHostName <<< { dnsName: _ } <<<
        convertMultiHostName_
    }
    relay

convertIpv6 :: Csl.Ipv6 -> T.Ipv6
convertIpv6 = T.Ipv6 <<< convertIpv6_

foreign import convertIpv6_ :: Csl.Ipv6 -> ByteArray

convertIpv4 :: Csl.Ipv4 -> T.Ipv4
convertIpv4 = T.Ipv4 <<< convertIpv4_

foreign import convertIpv4_ :: Csl.Ipv4 -> ByteArray

foreign import convertRelay_ :: forall a. ConvertRelayHelper a -> Csl.Relay -> a

foreign import convertSingleHostAddr_
  :: forall a
   . MaybeFfiHelper
  -> (Maybe Int -> Maybe Csl.Ipv4 -> Maybe Csl.Ipv6 -> a)
  -> Csl.SingleHostAddr
  -> a

foreign import convertSingleHostName_
  :: forall a
   . MaybeFfiHelper
  -> (Maybe Int -> String -> a)
  -> Csl.SingleHostName
  -> a

foreign import convertMultiHostName_
  :: Csl.MultiHostName
  -> String

convertPoolRetirement
  :: Csl.Ed25519KeyHash
  -> UInt
  -> T.Certificate
convertPoolRetirement poolKeyHash epoch = do
  T.PoolRetirement { poolKeyHash: wrap $ wrap poolKeyHash, epoch: wrap epoch }

convertMint :: Csl.Mint -> T.Mint
convertMint =
  let
    outerMap
      :: Array (Csl.ScriptHash /\ M.Map TokenName BigInt)
      -> M.Map Csl.CurrencySymbol (M.Map TokenName BigInt)
    outerMap = M.fromFoldable <<< map (lmap scriptHashAsCurrencySymbol)

    innerMap
      :: Array (Csl.ScriptHash /\ Csl.MintAssets)
      -> M.Map Csl.CurrencySymbol (M.Map TokenName BigInt)
    innerMap = outerMap <<< (map <<< map)
      ( M.fromFoldable <<< map convAssetName <<< _unpackMintAssets
          containerHelper
      )

    mintsMap
      :: Array (Csl.ScriptHash /\ Csl.MintsAssets)
      -> Array (Csl.ScriptHash /\ Csl.MintAssets)
    mintsMap ss = do
      (hash /\ assets) <- ss
      asset <- _unpackMintsAssets containerHelper assets
      pure (hash /\ asset)
  in
    T.Mint
      <<< mkNonAdaAsset
      <<< innerMap
      <<< mintsMap
      <<< _unpackMint containerHelper

  where
  convAssetName :: Csl.AssetName /\ Int.Int -> TokenName /\ BigInt
  convAssetName = bimap tokenNameFromAssetName Int.toBigInt

convertProtocolParamUpdate
  :: forall (r :: Row Type)
   . Csl.ProtocolParamUpdate
  -> Err r T.ProtocolParamUpdate
convertProtocolParamUpdate cslPpu = do
  let
    ppu = _unpackProtocolParamUpdate maybeFfiHelper cslPpu
    lbl = (<>) "ProtocolParamUpdate."
    minfeeA = Coin <<< BigNum.toBigInt <$> ppu.minfeeA
    minfeeB = Coin <<< BigNum.toBigInt <$> ppu.minfeeB
  maxBlockBodySize <- traverse (cslNumberToUInt (lbl "maxBlockBodySize"))
    ppu.maxBlockBodySize
  maxTxSize <- traverse (cslNumberToUInt (lbl "maxTxSize")) ppu.maxTxSize
  maxBlockHeaderSize <- traverse (cslNumberToUInt (lbl "maxBlockHeaderSize"))
    ppu.maxBlockHeaderSize
  let
    keyDeposit = Coin <<< BigNum.toBigInt <$> ppu.keyDeposit
    poolDeposit = Coin <<< BigNum.toBigInt <$> ppu.poolDeposit
  maxEpoch <- traverse (map T.Epoch <<< cslNumberToUInt (lbl "maxEpoch"))
    ppu.maxEpoch
  nOpt <- traverse (cslNumberToUInt (lbl "nOpt")) ppu.nOpt
  protocolVersion <- traverse (convertProtocolVersion (lbl "protocolVersion"))
    ppu.protocolVersion
  costModels <- addErrTrace (lbl "costModels") $ traverse convertCostModels
    ppu.costModels
  let
    maxTxExUnits = convertExUnits <$> ppu.maxTxExUnits
    maxBlockExUnits = convertExUnits <$>
      ppu.maxBlockExUnits
  maxValueSize <- traverse (cslNumberToUInt (lbl "maxValueSize"))
    ppu.maxValueSize
  collateralPercentage <- traverse
    (cslNumberToUInt (lbl "collateralPercentage"))
    ppu.collateralPercentage
  maxCollateralInputs <- traverse (cslNumberToUInt (lbl "maxCollateralInputs"))
    ppu.maxCollateralInputs
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
    , protocolVersion
    , minPoolCost: ppu.minPoolCost
    , adaPerUtxoByte: ppu.adaPerUtxoByte
    , costModels
    , executionCosts: convertExUnitPrices <$> ppu.executionCosts
    , maxTxExUnits
    , maxBlockExUnits
    , maxValueSize
    , collateralPercentage
    , maxCollateralInputs
    }

convertNonce :: Csl.Nonce -> T.Nonce
convertNonce = _convertNonce
  { hashNonce: T.HashNonce, identityNonce: T.IdentityNonce }

convertCostModels
  :: forall (r :: Row Type)
   . Csl.Costmdls
  -> Err r T.Costmdls
convertCostModels cslCostMdls =
  let
    mdls :: Array (Csl.Language /\ Csl.CostModel)
    mdls = _unpackCostModels containerHelper cslCostMdls
  in
    (T.Costmdls <<< M.fromFoldable) <$> traverse
      (bitraverse (pure <<< convertLanguage) convertCostModel)
      mdls

convertCostModel
  :: forall (r :: Row Type)
   . Csl.CostModel
  -> E (FromCslRepError + r) T.CostModel
convertCostModel = map T.CostModel <<< traverse stringToInt <<<
  _unpackCostModel
  where
  stringToInt
    :: String -> Either (Variant (fromCslRepError :: String | r)) Int.Int
  stringToInt s = cslErr (": string (" <> s <> ") -> int") $
    Int.fromBigInt =<< BigInt.fromString s

convertAuxiliaryData
  :: Csl.AuxiliaryData -> T.AuxiliaryData
convertAuxiliaryData ad = do
  let
    metadata = convertGeneralTransactionMetadata <$>
      _adGeneralMetadata maybeFfiHelper ad
  T.AuxiliaryData
    { metadata
    , nativeScripts: pure <<< convertNativeScripts =<< _adNativeScripts
        maybeFfiHelper
        ad
    , plutusScripts: pure <<< convertPlutusScripts =<< _adPlutusScripts
        maybeFfiHelper
        ad
    }

convertGeneralTransactionMetadata
  :: Csl.GeneralTransactionMetadata
  -> GeneralTransactionMetadata
convertGeneralTransactionMetadata gtm = wrap
  $ M.fromFoldable
  $ bimap (TransactionMetadatumLabel <<< BigNum.toBigInt) convertMetadatum
      <$> _unpackMetadatums containerHelper gtm

convertMetadatum :: Csl.TransactionMetadatum -> TransactionMetadatum
convertMetadatum tm = _convertMetadatum
  { from_bytes: Bytes
  , from_int: Int
  , from_text: Text
  , from_map: convertMetadataMap
  , from_list: convertMetadataList
  }
  tm

convertMetadataList
  :: Csl.MetadataList
  -> TransactionMetadatum
convertMetadataList ml = MetadataList
  $ convertMetadatum <$> _unpackMetadataList containerHelper ml

convertMetadataMap
  :: Csl.MetadataMap
  -> TransactionMetadatum
convertMetadataMap mm = MetadataMap
  $ M.fromFoldable
  $ bimap convertMetadatum convertMetadatum
      <$> _unpackMetadataMap containerHelper mm

-- unpack to array of tuples

---- conversion helpers

cslNumberToUInt
  :: forall (r :: Row Type). String -> Number -> E (FromCslRepError + r) UInt
cslNumberToUInt nm nb = cslErr (nm <> ": Number (" <> show nb <> ") -> UInt") $
  UInt.fromNumber' nb

cslRatioToRational
  :: forall (r :: Row Type)
   . { denominator :: Csl.BigNum, numerator :: Csl.BigNum }
  -> Ratio BigInt
cslRatioToRational { numerator, denominator } =
  reduce (BigNum.toBigInt numerator) (BigNum.toBigInt denominator)

convertExUnits
  :: forall (r :: Row Type)
   . Csl.ExUnits
  -> T.ExUnits
convertExUnits cslExunits =
  let
    { mem, steps } = _unpackExUnits cslExunits
  in
    { mem: _, steps: _ } (BigNum.toBigInt mem) (BigNum.toBigInt steps)

convertScriptDataHash :: Csl.ScriptDataHash -> T.ScriptDataHash
convertScriptDataHash = toBytes >>> unwrap >>> T.ScriptDataHash

convertProtocolVersion
  :: forall (r :: Row Type)
   . String
  -> Csl.ProtocolVersion
  -> E (FromCslRepError + r) T.ProtocolVersion
convertProtocolVersion nm cslPV =
  _unpackProtocolVersion cslPV #
    ( \{ major, minor } ->
        { major: _, minor: _ }
          <$> cslNumberToUInt (nm <> " major") major
          <*> cslNumberToUInt (nm <> " minor") minor
    )

---- foreign imports

foreign import _convertNonce
  :: { identityNonce :: T.Nonce, hashNonce :: ByteArray -> T.Nonce }
  -> Csl.Nonce
  -> T.Nonce

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
     , protocolVersion :: Maybe Csl.ProtocolVersion
     , minPoolCost :: Maybe Csl.BigNum
     , adaPerUtxoByte :: Maybe Csl.BigNum
     , costModels :: Maybe Csl.Costmdls
     , executionCosts :: Maybe Csl.ExUnitPrices
     , maxTxExUnits :: Maybe Csl.ExUnits
     , maxBlockExUnits :: Maybe Csl.ExUnits
     , maxValueSize :: Maybe Number
     , collateralPercentage :: Maybe Number
     , maxCollateralInputs :: Maybe Number
     }

foreign import _unpackCostModels
  :: ContainerHelper -> Csl.Costmdls -> Array (Csl.Language /\ Csl.CostModel)

foreign import _unpackCostModel :: Csl.CostModel -> Array String

foreign import _unpackMetadataMap
  :: ContainerHelper
  -> Csl.MetadataMap
  -> Array (Csl.TransactionMetadatum /\ Csl.TransactionMetadatum)

foreign import _unpackMetadataList
  :: ContainerHelper -> Csl.MetadataList -> Array Csl.TransactionMetadatum

type MetadatumHelper =
  { from_map :: Csl.MetadataMap -> TransactionMetadatum
  , from_list :: Csl.MetadataList -> TransactionMetadatum
  , from_int :: Csl.Int -> TransactionMetadatum
  , from_text :: String -> TransactionMetadatum
  , from_bytes :: ByteArray -> TransactionMetadatum
  }

foreign import _unpackProtocolVersion
  :: Csl.ProtocolVersion
  -> { major :: Number, minor :: Number }

foreign import _unpackExUnits
  :: Csl.ExUnits -> { mem :: Csl.BigNum, steps :: Csl.BigNum }

foreign import _unpackUnitInterval
  :: Csl.UnitInterval -> { numerator :: Csl.BigNum, denominator :: Csl.BigNum }

convertExUnitPrices :: Csl.ExUnitPrices -> T.ExUnitPrices
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
  :: MetadatumHelper
  -> Csl.TransactionMetadatum
  -> TransactionMetadatum

foreign import _unpackMetadatums
  :: ContainerHelper
  -> Csl.GeneralTransactionMetadata
  -> Array (Csl.BigNum /\ Csl.TransactionMetadatum)

foreign import _txBody :: Csl.Transaction -> Csl.TransactionBody
foreign import _txIsValid :: Csl.Transaction -> Boolean
foreign import _txWitnessSet :: Csl.Transaction -> Csl.TransactionWitnessSet
foreign import _txAuxiliaryData
  :: MaybeFfiHelper -> Csl.Transaction -> Maybe Csl.AuxiliaryData

foreign import _adGeneralMetadata
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe Csl.GeneralTransactionMetadata

foreign import _adNativeScripts
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe Csl.NativeScripts

foreign import _adPlutusScripts
  :: MaybeFfiHelper -> Csl.AuxiliaryData -> Maybe Csl.PlutusScripts

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
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.BigNum

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
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.AuxiliaryDataHash

-- validity_start_interval(): number | void
foreign import _txBodyValidityStartInterval
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.BigNum

-- mint(): Mint | void
foreign import _txBodyMint
  :: MaybeFfiHelper -> Csl.TransactionBody -> Maybe Csl.Mint

-- reference_inputs(): TransactionInputs | void;
foreign import _txBodyReferenceInputs
  :: MaybeFfiHelper
  -> ContainerHelper
  -> Csl.TransactionBody
  -> Maybe (Array Csl.TransactionInput)

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
  :: ContainerHelper
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe (Array Csl.Ed25519KeyHash)

-- network_id(): NetworkId | void
foreign import _txBodyNetworkId
  :: Csl.NetworkId
  -> Csl.NetworkId
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe Csl.NetworkId

-- collateral_return(): TransactionOutput | void
foreign import _txBodyCollateralReturn
  :: MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe Csl.TransactionOutput

-- total_collateral(): BigNum | void
foreign import _txBodyTotalCollateral
  :: MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe Csl.BigNum

foreign import _unpackWithdrawals
  :: ContainerHelper
  -> Csl.Withdrawals
  -> Array (Csl.RewardAddress /\ Csl.BigNum)

foreign import _unpackUpdate
  :: ContainerHelper
  -> Csl.Update
  -> { epoch :: Number
     , paramUpdates :: Array (Csl.GenesisHash /\ Csl.ProtocolParamUpdate)
     }

foreign import _unpackMint
  :: ContainerHelper -> Csl.Mint -> Array (Csl.ScriptHash /\ Csl.MintsAssets)

foreign import _unpackMintAssets
  :: ContainerHelper -> Csl.MintAssets -> Array (Csl.AssetName /\ Csl.Int)

foreign import _unpackMintsAssets
  :: ContainerHelper -> Csl.MintsAssets -> Array Csl.MintAssets

type CertConvHelper (r :: Type) =
  { stakeDeregistration :: Csl.StakeCredential -> r
  , stakeRegistration :: Csl.StakeCredential -> r
  , stakeDelegation ::
      Csl.StakeCredential -> Csl.Ed25519KeyHash -> r
  , poolRegistration :: Csl.PoolParams -> r
  , poolRetirement :: Csl.Ed25519KeyHash -> UInt -> r
  , genesisKeyDelegation ::
      Csl.GenesisHash
      -> Csl.GenesisDelegateHash
      -> Csl.VRFKeyHash
      -> r
  , moveInstantaneousRewardsToOtherPotCert ::
      Number -> Csl.BigNum -> r
  , moveInstantaneousRewardsToStakeCreds ::
      Number -> Csl.MIRToStakeCredentials -> r
  }

foreign import _convertCert
  :: CertConvHelper T.Certificate
  -> Csl.Certificate
  -> T.Certificate

foreign import poolParamsOperator :: Csl.PoolParams -> Csl.Ed25519KeyHash
foreign import poolParamsVrfKeyhash :: Csl.PoolParams -> Csl.VRFKeyHash
foreign import poolParamsPledge :: Csl.PoolParams -> Csl.BigNum
foreign import poolParamsCost :: Csl.PoolParams -> Csl.BigNum
foreign import poolParamsMargin :: Csl.PoolParams -> Csl.UnitInterval
foreign import poolParamsRewardAccount :: Csl.PoolParams -> Csl.RewardAddress
foreign import poolParamsPoolOwners
  :: ContainerHelper -> Csl.PoolParams -> Array Csl.Ed25519KeyHash

foreign import poolParamsRelays
  :: ContainerHelper -> Csl.PoolParams -> Array Csl.Relay

foreign import poolParamsPoolMetadata
  :: MaybeFfiHelper -> Csl.PoolParams -> Maybe Csl.PoolMetadata

foreign import unpackMIRToStakeCredentials_
  :: ContainerHelper
  -> Csl.MIRToStakeCredentials
  -> Array (Csl.StakeCredential /\ Csl.Int)

foreign import convertPoolMetadata_
  :: forall a. (String -> Csl.PoolMetadataHash -> a) -> Csl.PoolMetadata -> a
