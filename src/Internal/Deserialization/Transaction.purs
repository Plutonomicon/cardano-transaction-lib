module Ctl.Internal.Deserialization.Transaction
  ( convertAuxiliaryData
  , convertCertificate
  , convertCostModel
  , convertCostModels
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

import Cardano.Serialization.Lib
  ( fromBytes
  , toBytes
  , transactionBody_certs
  , transactionBody_fee
  , transactionBody_inputs
  , transactionBody_outputs
  , transactionBody_referenceInputs
  , transactionBody_update
  , transactionBody_withdrawals
  , unpackListContainer
  , unpackMapContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum) as Csl
import Cardano.Types.BigNum (toBigInt) as BigNum
import Cardano.Types.MultiAsset (MultiAsset(..))
import Cardano.Types.Slot (Slot(..))
import Cardano.Types.TransactionInput as TransactionInput
import Cardano.Types.TransactionOutput as TransactionOutput
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
import Ctl.Internal.Cardano.Types.Value (Coin(Coin))
import Ctl.Internal.Deserialization.Error
  ( Err
  , FromCslRepError
  , addErrTrace
  , cslErr
  )
import Ctl.Internal.Deserialization.Language (convertLanguage)
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
import Ctl.Internal.Helpers (notImplemented)
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
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Int (Int) as Csl
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.RewardAddress (RewardAddress(RewardAddress)) as T
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata
  , TransactionMetadatum(MetadataList, MetadataMap, Bytes, Int, Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash(VRFKeyHash)) as T
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.ByteArray (ByteArray)
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Ratio (Ratio, reduce)
import Data.Set (fromFoldable) as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Variant (Variant)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Type.Row (type (+))

-- | Deserializes CBOR encoded transaction to a CTL's native type.
deserializeTransaction
  :: forall (r :: Row Type). CborBytes -> Err r T.Transaction
deserializeTransaction txCbor =
  cslErr "TransactionOutput" (fromBytes (unwrap txCbor)) >>=
    convertTransaction

-- | Converts transaction from foreign CSL representation to CTL's one.
convertTransaction
  :: forall (r :: Row Type). Csl.Transaction -> Err r T.Transaction
convertTransaction tx = notImplemented -- addErrTrace "convertTransaction" do

-- body <- convertTxBody $ _txBody tx
-- let
--   witnessSet = convertWitnessSet (_txWitnessSet tx)
--   auxiliaryData = convertAuxiliaryData <$>
--     _txAuxiliaryData maybeFfiHelper tx
-- pure $ T.Transaction
--   { body
--   , witnessSet
--   , isValid: _txIsValid tx
--   , auxiliaryData
--   }

-- | Converts transaction body from foreign CSL representation to CTL's one.
convertTxBody :: forall (r :: Row Type). Csl.TransactionBody -> Err r T.TxBody
convertTxBody txBody = notImplemented -- do

-- let
--   inputs = Set.fromFoldable $ TransactionInput.fromCsl <$> unpackListContainer (transactionBody_inputs txBody)

-- outputs <-
--   unpackListContainer (transactionBody_outputs txBody)
--     # traverse (TransactionOutput.fromCsl >>> cslErr "TransactionOutput")
-- let
--   fee = Coin $ transactionBody_fee txBody
--   networkId =
--     notImplemented

--   ws :: Maybe (Array (Csl.RewardAddress /\ Csl.BigNum))
--   ws = unpackMapContainer <$> transactionBody_withdrawals txBody

-- withdrawals :: Maybe (M.Map T.RewardAddress Coin) <-
--   -- array -> map
--   (map <<< map) (M.fromFoldable <<< map (lmap T.RewardAddress))
--     -- bignum -> coin
--     <<< (traverse <<< traverse <<< traverse)
--       (BigNum.toBigInt >>> Coin >>> pure)
--     $ ws

-- update <- traverse convertUpdate $ transactionBody_update txBody

-- let
--   cslReferenceInputs :: Array Csl.TransactionInput
--   cslReferenceInputs =
--     map unpackListContainer (toMaybe (transactionBody_referenceInputs txBody))
--       # fromMaybe mempty

--   referenceInputs = Set.fromFoldable $ TransactionInput.fromCsl <$> cslReferenceInputs

--   certs = map convertCertificate <$>
--     toMaybe (transactionBody_certs txBody)

-- collateralReturn <- notImplemented
--   -- _txBodyCollateralReturn maybeFfiHelper txBody #
--   --   traverse (TransactionOutput.fromCsl >>> cslErr "TransactionOutput")

-- let
--   totalCollateral = notImplemented -- _txBodyTotalCollateral maybeFfiHelper txBody <#>
--     -- (BigNum.toBigInt >>> Coin)

-- pure $ notImplemented
-- T.TxBody
--   { inputs
--   , outputs
--   , fee
--   , ttl: Slot <$> notImplemented -- _txBodyTtl maybeFfiHelper txBody
--   , certs
--   , withdrawals
--   , update
--   , auxiliaryDataHash:
--       T.AuxiliaryDataHash <<< toBytes <$>
--         notImplemented -- _txBodyAuxiliaryDataHash maybeFfiHelper txBody
--   , validityStartInterval:
--       Slot <$> _txBodyValidityStartInterval maybeFfiHelper txBody
--   , mint: map convertMint $ _txBodyMultiAssets maybeFfiHelper txBody
--   , referenceInputs
--   , scriptDataHash: convertScriptDataHash <$> _txBodyScriptDataHash
--       maybeFfiHelper
--       txBody
--   , collateral: _txBodyCollateral containerHelper maybeFfiHelper txBody >>=
--       map TransactionInput.fromCsl >>> pure
--   , requiredSigners:
--       _txBodyRequiredSigners containerHelper maybeFfiHelper txBody #
--         (map <<< map) (T.RequiredSigner <<< wrap)
--   , networkId
--   , collateralReturn
--   , totalCollateral
--   }

convertUpdate :: forall (r :: Row Type). Csl.Update -> Err r T.Update
convertUpdate u = notImplemented -- do

-- let { epoch: e, paramUpdates } = _unpackUpdate containerHelper u
-- epoch <- map T.Epoch $ cslNumberToUInt "convertUpdate: epoch" e
-- ppus <- traverse
--   ( bitraverse
--       (pure <<< T.GenesisHash <<< toBytes)
--       convertProtocolParamUpdate
--   )
--   paramUpdates
-- pure
--   { epoch
--   , proposedProtocolParameterUpdates: T.ProposedProtocolParameterUpdates $
--       M.fromFoldable ppus
--   }

convertCertificate
  :: Csl.Certificate -> T.Certificate
convertCertificate = notImplemented -- _convertCert certConvHelper

-- where
-- certConvHelper :: CertConvHelper T.Certificate
-- certConvHelper =
--   { stakeDeregistration: T.StakeDeregistration
--   , stakeRegistration: T.StakeRegistration
--   , stakeDelegation: \sc -> T.StakeDelegation sc <<< wrap <<< wrap <<< wrap
--   , poolRegistration: convertPoolRegistration
--   , poolRetirement: convertPoolRetirement
--   , genesisKeyDelegation: \genesisHash genesisDelegateHash vrfKeyhash -> do
--       T.GenesisKeyDelegation
--         { genesisHash: T.GenesisHash $ toBytes genesisHash
--         , genesisDelegateHash: T.GenesisDelegateHash $ toBytes
--             genesisDelegateHash
--         , vrfKeyhash: T.VRFKeyHash vrfKeyhash
--         }
--   , moveInstantaneousRewardsToOtherPotCert: \pot amount -> do
--       T.MoveInstantaneousRewardsCert $
--         T.ToOtherPot { pot, amount: amount }
--   , moveInstantaneousRewardsToStakeCreds: \pot amounts -> do
--       T.MoveInstantaneousRewardsCert $
--         T.ToStakeCreds { pot, amounts: convertMIRToStakeCredentials amounts }
--   }

convertMIRToStakeCredentials
  :: Csl.MIRToStakeCredentials -> T.MIRToStakeCredentials
convertMIRToStakeCredentials = notImplemented

-- T.MIRToStakeCredentials <<< M.fromFoldable <<< unpackMIRToStakeCredentials_
--   containerHelper

convertPoolRegistration :: Csl.PoolParams -> T.Certificate
convertPoolRegistration params = notImplemented -- do

-- let
--   relays = convertRelay <$> poolParamsRelays containerHelper params
-- T.PoolRegistration
--   { operator: PoolPubKeyHash $ wrap $ wrap $ poolParamsOperator params
--   , vrfKeyhash: T.VRFKeyHash $ poolParamsVrfKeyhash params
--   , pledge: poolParamsPledge params
--   , cost: poolParamsCost params
--   , margin: _unpackUnitInterval $ poolParamsMargin params
--   , rewardAccount: T.RewardAddress $ poolParamsRewardAccount params
--   , poolOwners: wrap <<< wrap <<< wrap <$> poolParamsPoolOwners
--       containerHelper
--       params
--   , relays
--   , poolMetadata: poolParamsPoolMetadata maybeFfiHelper params <#>
--       convertPoolMetadata_
--         \url hash -> T.PoolMetadata
--           { url: T.URL url
--           , hash: T.PoolMetadataHash $ toBytes hash
--           }
--   }

convertRelay :: Csl.Relay -> T.Relay
convertRelay relay = notImplemented -- do

-- convertRelay_
--   { asSingleHostAddr: convertSingleHostAddr_ maybeFfiHelper
--       \mbPort mbIpv4 mbIpv6 -> do
--         let
--           ipv4 = mbIpv4 <#> convertIpv4
--           ipv6 = mbIpv6 <#> convertIpv6
--         T.SingleHostAddr { port: mbPort, ipv4, ipv6 }
--   , asSingleHostName: convertSingleHostName_ maybeFfiHelper
--       \port mbHost -> T.SingleHostName { port, dnsName: mbHost }
--   , asMultiHostName: T.MultiHostName <<< { dnsName: _ } <<<
--       convertMultiHostName_
--   }
--   relay

convertIpv6 :: Csl.Ipv6 -> T.Ipv6
convertIpv6 = notImplemented -- T.Ipv6 <<< convertIpv6_

convertIpv4 :: Csl.Ipv4 -> T.Ipv4
convertIpv4 = notImplemented -- T.Ipv4 <<< convertIpv4_

convertPoolRetirement
  :: Csl.Ed25519KeyHash
  -> UInt
  -> T.Certificate
convertPoolRetirement poolKeyHash epoch = notImplemented -- do

-- T.PoolRetirement
--   { poolKeyHash: wrap $ wrap $ wrap $ poolKeyHash, epoch: wrap epoch }

convertMint :: Csl.Mint -> T.Mint
convertMint = -- T.Mint <<< MultiAsset <<<
  -- unpackMapContainerToMapWith (wrap)
  --   ( unpackMapContainerToMapWith
  --       identity
  --       (Int.toBigInt <<< wrap)
  --   )
  notImplemented

convertProtocolParamUpdate
  :: forall (r :: Row Type)
   . Csl.ProtocolParamUpdate
  -> Err r T.ProtocolParamUpdate
convertProtocolParamUpdate cslPpu = notImplemented -- do

-- let
--   ppu = _unpackProtocolParamUpdate maybeFfiHelper cslPpu
--   lbl = (<>) "ProtocolParamUpdate."
--   minfeeA = Coin <<< BigNum.toBigInt <$> ppu.minfeeA
--   minfeeB = Coin <<< BigNum.toBigInt <$> ppu.minfeeB
-- maxBlockBodySize <- traverse (cslNumberToUInt (lbl "maxBlockBodySize"))
--   ppu.maxBlockBodySize
-- maxTxSize <- traverse (cslNumberToUInt (lbl "maxTxSize")) ppu.maxTxSize
-- maxBlockHeaderSize <- traverse (cslNumberToUInt (lbl "maxBlockHeaderSize"))
--   ppu.maxBlockHeaderSize
-- let
--   keyDeposit = Coin <<< BigNum.toBigInt <$> ppu.keyDeposit
--   poolDeposit = Coin <<< BigNum.toBigInt <$> ppu.poolDeposit
-- maxEpoch <- traverse (map T.Epoch <<< cslNumberToUInt (lbl "maxEpoch"))
--   ppu.maxEpoch
-- nOpt <- traverse (cslNumberToUInt (lbl "nOpt")) ppu.nOpt
-- protocolVersion <- traverse (convertProtocolVersion (lbl "protocolVersion"))
--   ppu.protocolVersion
-- costModels <- addErrTrace (lbl "costModels") $ traverse convertCostModels
--   ppu.costModels
-- let
--   maxTxExUnits = convertExUnits <$> ppu.maxTxExUnits
--   maxBlockExUnits = convertExUnits <$>
--     ppu.maxBlockExUnits
-- maxValueSize <- traverse (cslNumberToUInt (lbl "maxValueSize"))
--   ppu.maxValueSize
-- collateralPercentage <- traverse
--   (cslNumberToUInt (lbl "collateralPercentage"))
--   ppu.collateralPercentage
-- maxCollateralInputs <- traverse (cslNumberToUInt (lbl "maxCollateralInputs"))
--   ppu.maxCollateralInputs
-- pure
--   { minfeeA
--   , minfeeB
--   , maxBlockBodySize
--   , maxTxSize
--   , maxBlockHeaderSize
--   , keyDeposit
--   , poolDeposit
--   , maxEpoch
--   , nOpt
--   , poolPledgeInfluence: _unpackUnitInterval <$> ppu.poolPledgeInfluence
--   , expansionRate: _unpackUnitInterval <$> ppu.expansionRate
--   , treasuryGrowthRate: _unpackUnitInterval <$> ppu.treasuryGrowthRate
--   , protocolVersion
--   , minPoolCost: ppu.minPoolCost
--   , adaPerUtxoByte: ppu.adaPerUtxoByte
--   , costModels
--   , executionCosts: convertExUnitPrices <$> ppu.executionCosts
--   , maxTxExUnits
--   , maxBlockExUnits
--   , maxValueSize
--   , collateralPercentage
--   , maxCollateralInputs
--   }

convertNonce :: Csl.Nonce -> T.Nonce
convertNonce = notImplemented -- _convertNonce

-- { hashNonce: T.HashNonce, identityNonce: T.IdentityNonce }

convertCostModels
  :: forall (r :: Row Type)
   . Csl.Costmdls
  -> Err r T.Costmdls
convertCostModels cslCostMdls = notImplemented

-- let
--   mdls :: Array (Csl.Language /\ Csl.CostModel)
--   mdls = _unpackCostModels containerHelper cslCostMdls
-- in
--   (T.Costmdls <<< M.fromFoldable) <$> traverse
--     (bitraverse (pure <<< convertLanguage) convertCostModel)
--     mdls

convertCostModel
  :: forall (r :: Row Type)
   . Csl.CostModel
  -> E (FromCslRepError + r) T.CostModel
convertCostModel = notImplemented

-- map T.CostModel <<< traverse stringToInt <<<
--   _unpackCostModel
--   where
--   stringToInt
--     :: String -> Either (Variant (fromCslRepError :: String | r)) Int.Int
--   stringToInt s = cslErr (": string (" <> s <> ") -> int") $
--     Int.fromBigInt =<< BigInt.fromString s

convertAuxiliaryData
  :: Csl.AuxiliaryData -> T.AuxiliaryData
convertAuxiliaryData ad = notImplemented -- do

-- let
--   metadata = convertGeneralTransactionMetadata <$>
--     _adGeneralMetadata maybeFfiHelper ad
-- T.AuxiliaryData
--   { metadata
--   , nativeScripts: pure <<< convertNativeScripts =<< _adNativeScripts
--       maybeFfiHelper
--       ad
--   , plutusScripts: pure <<< convertPlutusScripts =<< _adPlutusScripts
--       maybeFfiHelper
--       ad
--   }

convertGeneralTransactionMetadata
  :: Csl.GeneralTransactionMetadata
  -> GeneralTransactionMetadata
convertGeneralTransactionMetadata gtm = notImplemented -- wrap

-- $ M.fromFoldable
-- $ bimap (TransactionMetadatumLabel <<< BigNum.toBigInt) convertMetadatum
--     <$> _unpackMetadatums containerHelper gtm

convertMetadatum :: Csl.TransactionMetadatum -> TransactionMetadatum
convertMetadatum tm = notImplemented -- _convertMetadatum

-- { from_bytes: Bytes
-- , from_int: Int
-- , from_text: Text
-- , from_map: convertMetadataMap
-- , from_list: convertMetadataList
-- }
-- tm

convertMetadataList
  :: Csl.MetadataList
  -> TransactionMetadatum
convertMetadataList ml = notImplemented -- MetadataList

-- $ convertMetadatum <$> _unpackMetadataList containerHelper ml

convertMetadataMap
  :: Csl.MetadataMap
  -> TransactionMetadatum
convertMetadataMap mm = notImplemented -- MetadataMap

-- $ M.fromFoldable
-- $ bimap convertMetadatum convertMetadatum
--     <$> _unpackMetadataMap containerHelper mm

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
convertExUnits cslExunits = notImplemented

-- let
--   { mem, steps } = _unpackExUnits cslExunits
-- in
--   { mem: _, steps: _ } (BigNum.toBigInt mem) (BigNum.toBigInt steps)

convertScriptDataHash :: Csl.ScriptDataHash -> T.ScriptDataHash
convertScriptDataHash = toBytes >>> T.ScriptDataHash

convertProtocolVersion
  :: forall (r :: Row Type)
   . String
  -> Csl.ProtocolVersion
  -> E (FromCslRepError + r) T.ProtocolVersion
convertProtocolVersion nm cslPV = notImplemented
-- _unpackProtocolVersion cslPV #
--   ( \{ major, minor } ->
--       { major: _, minor: _ }
--         <$> cslNumberToUInt (nm <> " major") major
--         <*> cslNumberToUInt (nm <> " minor") minor
--   )
