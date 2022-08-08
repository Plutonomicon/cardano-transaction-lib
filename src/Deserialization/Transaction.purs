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
  , convertLanguage
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
  , cslIntToUInt
  , cslRatioToRational
  , deserializeTransaction
  ) where

import Prelude

import Cardano.Types.Transaction
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
  , Language(PlutusV1)
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
import Cardano.Types.Value
  ( Coin(Coin)
  , NonAdaAsset(NonAdaAsset)
  , scriptHashAsCurrencySymbol
  )
import Control.Lazy (fix)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Bitraversable (bitraverse)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.Ratio (Ratio, reduce)
import Data.Set (fromFoldable) as Set
import Data.Traversable (traverse, for)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Variant (Variant, inj)
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
import Error (E)
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
  , StakeCredential
  , NetworkId(TestnetId, MainnetId)
  ) as Csl
import Serialization.Address (Slot(Slot))
import Serialization.Hash (Ed25519KeyHash, ScriptHash)
import Serialization.Types
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
  , MultiHostName
  , NativeScripts
  , Nonce
  , PlutusScripts
  , PoolMetadata
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
  , VRFKeyHash
  , Withdrawals
  ) as Csl
import Type.Row (type (+))
import Types.BigNum (BigNum) as Csl
import Types.BigNum (toBigInt') as BigNum
import Types.ByteArray (ByteArray)
import Types.CborBytes (CborBytes)
import Types.Int (Int) as Csl
import Types.Int as Int
import Types.TokenName (TokenName, tokenNameFromAssetName)
import Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatum(MetadataList, MetadataMap, Bytes, Int, Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Untagged.Union (asOneOf)

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
  auxiliaryData <- traverse convertAuxiliaryData
    (_txAuxiliaryData maybeFfiHelper tx)
  pure $ T.Transaction
    { body
    , witnessSet
    , isValid: _txIsValid tx
    , auxiliaryData
    }

-- | Converts transaction body from foreign CSL representation to CTL's one.
convertTxBody :: forall (r :: Row Type). Csl.TransactionBody -> Err r T.TxBody
convertTxBody txBody = do
  inputs <-
    _txBodyInputs containerHelper txBody
      # traverse (convertInput >>> cslErr "TransactionInput")
      <#> Set.fromFoldable
  outputs <-
    _txBodyOutputs containerHelper txBody
      # traverse (convertOutput >>> cslErr "TransactionOutput")
  fee <-
    Coin <$> (_txBodyFee txBody # BigNum.toBigInt' "Tx fee")
  let
    networkId =
      _txBodyNetworkId Csl.TestnetId Csl.MainnetId maybeFfiHelper txBody

    ws :: Maybe (Array (Csl.RewardAddress /\ Csl.BigNum))
    ws = _unpackWithdrawals containerHelper <$> _txBodyWithdrawals
      maybeFfiHelper
      txBody

  withdrawals :: Maybe (M.Map Csl.RewardAddress Coin) <-
    -- array -> map
    (map <<< map) M.fromFoldable
      -- bignum -> coin
      <<< (traverse <<< traverse <<< traverse)
        (BigNum.toBigInt' "txbody withdrawals" >>> map Coin)
      $ ws

  update <- traverse convertUpdate $ _txBodyUpdate maybeFfiHelper txBody

  certs <- addErrTrace "Tx body certificates"
    $ traverse (traverse convertCertificate)
    $ _txBodyCerts containerHelper
        maybeFfiHelper
        txBody

  pure $ T.TxBody
    { inputs
    , outputs
    , fee
    , ttl: Slot <$> _txBodyTtl maybeFfiHelper txBody
    , certs
    , withdrawals
    , update
    , auxiliaryDataHash:
        T.AuxiliaryDataHash <<< toBytes <<< asOneOf <$>
          _txBodyAuxiliaryDataHash maybeFfiHelper txBody
    , validityStartInterval:
        Slot <$> _txBodyValidityStartInterval maybeFfiHelper txBody
    , mint: map convertMint $ _txBodyMultiAssets maybeFfiHelper txBody
    , scriptDataHash: convertScriptDataHash <$> _txBodyScriptDataHash
        maybeFfiHelper
        txBody
    , collateral: _txBodyCollateral containerHelper maybeFfiHelper txBody >>=
        traverse convertInput
    , requiredSigners:
        _txBodyRequiredSigners containerHelper maybeFfiHelper txBody #
          (map <<< map) T.RequiredSigner
    , networkId
    }

convertUpdate :: forall (r :: Row Type). Csl.Update -> Err r T.Update
convertUpdate u = do
  let { epoch: e, paramUpdates } = _unpackUpdate containerHelper u
  epoch <- map T.Epoch $ cslNumberToUInt "convertUpdate: epoch" e
  ppus <- traverse
    ( bitraverse
        (pure <<< T.GenesisHash <<< toBytes <<< asOneOf)
        convertProtocolParamUpdate
    )
    paramUpdates
  pure
    { epoch
    , proposedProtocolParameterUpdates: T.ProposedProtocolParameterUpdates $
        M.fromFoldable ppus
    }

convertCertificate
  :: forall (r :: Row Type). Csl.Certificate -> Err r T.Certificate
convertCertificate = _convertCert certConvHelper
  where
  certConvHelper :: CertConvHelper (Err r T.Certificate)
  certConvHelper =
    { stakeDeregistration: pure <<< T.StakeDeregistration
    , stakeRegistration: pure <<< T.StakeRegistration
    , stakeDelegation: \sc -> pure <<< T.StakeDelegation sc
    , poolRegistration: convertPoolRegistration
    , poolRetirement: convertPoolRetirement
    , genesisKeyDelegation: \genesisHash genesisDelegateHash vrfKeyhash -> do
        pure $ T.GenesisKeyDelegation
          { genesisHash: T.GenesisHash $ toBytes $ asOneOf genesisHash
          , genesisDelegateHash: T.GenesisDelegateHash
              (toBytes $ asOneOf genesisDelegateHash)
          , vrfKeyhash: vrfKeyhash
          }
    , moveInstantaneousRewardsToOtherPotCert: \pot amount -> do
        pure $ T.MoveInstantaneousRewardsCert $
          T.ToOtherPot { pot, amount: amount }
    , moveInstantaneousRewardsToStakeCreds: \pot amounts -> do
        pure $ T.MoveInstantaneousRewardsCert $
          T.ToStakeCreds { pot, amounts: convertMIRToStakeCredentials amounts }
    }

convertMIRToStakeCredentials
  :: Csl.MIRToStakeCredentials -> T.MIRToStakeCredentials
convertMIRToStakeCredentials =
  T.MIRToStakeCredentials <<< M.fromFoldable <<< unpackMIRToStakeCredentials_
    containerHelper

convertPoolRegistration
  :: forall (r :: Row Type)
   . Csl.PoolParams
  -> Err r T.Certificate
convertPoolRegistration params = do
  relays <- traverse convertRelay $ poolParamsRelays containerHelper params
  pure $ T.PoolRegistration
    { operator: poolParamsOperator params
    , vrfKeyhash: poolParamsVrfKeyhash params
    , pledge: poolParamsPledge params
    , cost: poolParamsCost params
    , margin: _unpackUnitInterval $ poolParamsMargin params
    , rewardAccount: poolParamsRewardAccount params
    , poolOwners: poolParamsPoolOwners containerHelper params
    , relays
    , poolMetadata: poolParamsPoolMetadata maybeFfiHelper params <#>
        convertPoolMetadata_
          \url hash -> T.PoolMetadata
            { url: T.URL url, hash: T.PoolMetadataHash hash }
    }

type ConvertRelayHelper a =
  { asSingleHostAddr :: Csl.SingleHostAddr -> a
  , asSingleHostName :: Csl.SingleHostName -> a
  , asMultiHostName :: Csl.MultiHostName -> a
  }

convertRelay
  :: forall (r :: Row Type). Csl.Relay -> Err r T.Relay
convertRelay relay = addErrTrace "Relay" do
  convertRelay_
    { asSingleHostAddr: convertSingleHostAddr_ maybeFfiHelper
        \mbPort mbIpv4 mbIpv6 -> do
          ipv4 <- for mbIpv4 convertIpv4
          ipv6 <- for mbIpv6 convertIpv6
          pure $ T.SingleHostAddr { port: mbPort, ipv4, ipv6 }
    , asSingleHostName: convertSingleHostName_ maybeFfiHelper
        \port mbHost -> pure $ T.SingleHostName { port, dnsName: mbHost }
    , asMultiHostName: pure <<< T.MultiHostName <<< { dnsName: _ } <<<
        convertMultiHostName_
    }
    relay

convertIpv6 :: forall (r :: Row Type). Csl.Ipv6 -> Err r T.Ipv6
convertIpv6 = pure <<< T.Ipv6 <<< convertIpv6_

foreign import convertIpv6_ :: Csl.Ipv6 -> ByteArray

convertIpv4 :: forall (r :: Row Type). Csl.Ipv4 -> Err r T.Ipv4
convertIpv4 = pure <<< T.Ipv4 <<< convertIpv4_

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
  :: forall (r :: Row Type)
   . Ed25519KeyHash
  -> Int
  -> Err r T.Certificate
convertPoolRetirement poolKeyhash epochInt = do
  epoch <- wrap <$> cslIntToUInt "PoolRetirement.epoch" epochInt
  pure $ T.PoolRetirement { poolKeyhash, epoch }

convertMint :: Csl.Mint -> T.Mint
convertMint mint = T.Mint $ NonAdaAsset
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

  minfeeA <- traverse (map Coin <<< BigNum.toBigInt' (lbl "minfeeA"))
    ppu.minfeeA
  minfeeB <- traverse (map Coin <<< BigNum.toBigInt' (lbl "minfeeB"))
    ppu.minfeeB
  maxBlockBodySize <- traverse (cslNumberToUInt (lbl "maxBlockBodySize"))
    ppu.maxBlockBodySize
  maxTxSize <- traverse (cslNumberToUInt (lbl "maxTxSize")) ppu.maxTxSize
  maxBlockHeaderSize <- traverse (cslNumberToUInt (lbl "maxBlockHeaderSize"))
    ppu.maxBlockHeaderSize
  keyDeposit <- traverse (map Coin <<< BigNum.toBigInt' (lbl "keyDeposit"))
    ppu.keyDeposit
  poolDeposit <- traverse (map Coin <<< BigNum.toBigInt' (lbl "poolDeposit"))
    ppu.poolDeposit
  maxEpoch <- traverse (map T.Epoch <<< cslNumberToUInt (lbl "maxEpoch"))
    ppu.maxEpoch
  nOpt <- traverse (cslNumberToUInt (lbl "nOpt")) ppu.nOpt
  protocolVersion <- traverse (convertProtocolVersion (lbl "protocolVersion"))
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

convertNonce :: Csl.Nonce -> T.Nonce
convertNonce = _convertNonce
  { hashNonce: T.HashNonce, identityNonce: T.IdentityNonce }

convertCostModels
  :: forall (r :: Row Type)
   . String
  -> Csl.Costmdls
  -> E (FromCslRepError + r) T.Costmdls
convertCostModels nm cslCostMdls =
  let
    mdls :: Array (Csl.Language /\ Csl.CostModel)
    mdls = _unpackCostModels containerHelper cslCostMdls
  in
    (T.Costmdls <<< M.fromFoldable) <$> traverse
      (bitraverse (convertLanguage nm) (convertCostModel nm))
      mdls

convertLanguage
  :: forall (r :: Row Type)
   . String
  -> Csl.Language
  -> E (FromCslRepError + r) T.Language
convertLanguage err = _convertLanguage
  (errorHelper (inj _fromCslRepError <<< \e -> err <> ": " <> e))
  { plutusV1: T.PlutusV1 }

convertCostModel
  :: forall (r :: Row Type)
   . String
  -> Csl.CostModel
  -> E (FromCslRepError + r) T.CostModel
convertCostModel err = map T.CostModel <<< traverse stringToInt <<<
  _unpackCostModel
  where
  stringToInt
    :: String -> Either (Variant (fromCslRepError :: String | r)) Int
  stringToInt s = cslErr (err <> ": string (" <> s <> ") -> int") $
    fromString s

convertAuxiliaryData
  :: forall (r :: Row Type). Csl.AuxiliaryData -> Err r T.AuxiliaryData
convertAuxiliaryData ad = addErrTrace "convertAuxiliaryData" do
  metadata <- traverse convertGeneralTransactionMetadata
    (_adGeneralMetadata maybeFfiHelper ad)
  pure $ T.AuxiliaryData
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
            ( map TransactionMetadatumLabel <<< BigNum.toBigInt'
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

cslIntToUInt
  :: forall (r :: Row Type). String -> Int -> E (FromCslRepError + r) UInt
cslIntToUInt nm nb = cslErr (nm <> ": Int (" <> show nb <> ") -> UInt") $
  UInt.fromInt' nb

cslRatioToRational
  :: forall (r :: Row Type)
   . String
  -> { denominator :: Csl.BigNum, numerator :: Csl.BigNum }
  -> E (FromCslRepError + r) (Ratio BigInt)
cslRatioToRational err { numerator, denominator } = reduce
  <$> BigNum.toBigInt' (err <> " cslRatioToRational") numerator
  <*> BigNum.toBigInt' (err <> " cslRatioToRational") denominator

convertExUnits
  :: forall (r :: Row Type)
   . String
  -> Csl.ExUnits
  -> E (FromCslRepError + r) T.ExUnits
convertExUnits nm cslExunits =
  let
    { mem, steps } = _unpackExUnits cslExunits
  in
    { mem: _, steps: _ }
      <$> BigNum.toBigInt' (nm <> " mem") mem
      <*> BigNum.toBigInt' (nm <> " steps") steps

convertScriptDataHash :: Csl.ScriptDataHash -> T.ScriptDataHash
convertScriptDataHash = asOneOf >>> toBytes >>> T.ScriptDataHash

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
     , d :: Maybe Csl.UnitInterval
     , extraEntropy :: Maybe Csl.Nonce
     , protocolVersion :: Maybe Csl.ProtocolVersion
     , minPoolCost :: Maybe Csl.BigNum
     , adaPerUtxoByte :: Maybe Csl.BigNum
     , costModels :: Maybe Csl.Costmdls
     , executionCosts :: Maybe Csl.ExUnitPrices
     , maxTxExUnits :: Maybe Csl.ExUnits
     , maxBlockExUnits :: Maybe Csl.ExUnits
     , maxValueSize :: Maybe Number
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

type MetadatumHelper (r :: Row Type) =
  { from_map :: Csl.MetadataMap -> Err r TransactionMetadatum
  , from_list :: Csl.MetadataList -> Err r TransactionMetadatum
  , from_int :: String -> Err r TransactionMetadatum
  , from_text :: String -> Err r TransactionMetadatum
  , from_bytes :: ByteArray -> Err r TransactionMetadatum
  , error :: String -> Err r TransactionMetadatum
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
  :: forall (r :: Row Type)
   . MetadatumHelper r
  -> Csl.TransactionMetadatum
  -> E (FromCslRepError + r) TransactionMetadatum

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
  :: ContainerHelper
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe (Array Ed25519KeyHash)

-- network_id(): NetworkId | void
foreign import _txBodyNetworkId
  :: Csl.NetworkId
  -> Csl.NetworkId
  -> MaybeFfiHelper
  -> Csl.TransactionBody
  -> Maybe Csl.NetworkId

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
  :: ContainerHelper -> Csl.Mint -> Array (ScriptHash /\ Csl.MintAssets)

foreign import _unpackMintAssets
  :: ContainerHelper -> Csl.MintAssets -> Array (Csl.AssetName /\ Csl.Int)

type CertConvHelper (r :: Type) =
  { stakeDeregistration :: Csl.StakeCredential -> r
  , stakeRegistration :: Csl.StakeCredential -> r
  , stakeDelegation ::
      Csl.StakeCredential -> Ed25519KeyHash -> r
  , poolRegistration :: Csl.PoolParams -> r
  , poolRetirement :: Ed25519KeyHash -> Int -> r
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
  :: forall (r :: Row Type)
   . CertConvHelper (Err r T.Certificate)
  -> Csl.Certificate
  -> Err r T.Certificate

foreign import _convertLanguage
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> { plutusV1 :: T.Language }
  -> Csl.Language
  -> E r T.Language

foreign import poolParamsOperator :: Csl.PoolParams -> Ed25519KeyHash
foreign import poolParamsVrfKeyhash :: Csl.PoolParams -> Csl.VRFKeyHash
foreign import poolParamsPledge :: Csl.PoolParams -> Csl.BigNum
foreign import poolParamsCost :: Csl.PoolParams -> Csl.BigNum
foreign import poolParamsMargin :: Csl.PoolParams -> Csl.UnitInterval
foreign import poolParamsRewardAccount :: Csl.PoolParams -> Csl.RewardAddress
foreign import poolParamsPoolOwners
  :: ContainerHelper -> Csl.PoolParams -> Array Ed25519KeyHash

foreign import poolParamsRelays
  :: ContainerHelper -> Csl.PoolParams -> Array Csl.Relay

foreign import poolParamsPoolMetadata
  :: MaybeFfiHelper -> Csl.PoolParams -> Maybe Csl.PoolMetadata

foreign import unpackMIRToStakeCredentials_
  :: ContainerHelper
  -> Csl.MIRToStakeCredentials
  -> Array (Csl.StakeCredential /\ Csl.Int)

foreign import convertPoolMetadata_
  :: forall a. (String -> ByteArray -> a) -> Csl.PoolMetadata -> a
