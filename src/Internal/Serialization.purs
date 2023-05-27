module Ctl.Internal.Serialization
  ( convertExUnitPrices
  , convertTransaction
  , convertTxBody
  , convertTxInput
  , convertTxOutput
  , defaultCostmdls
  , convertTransactionUnspentOutput
  , convertValue
  , serializeData
  , hashScriptData
  , hashTransaction
  , publicKeyHash
  , makeVkeywitness
  , module Ctl.Internal.Serialization.ToBytes
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  ) as T
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , GenesisKeyDelegation
      , MoveInstantaneousRewardsCert
      )
  , CostModel(CostModel)
  , Costmdls(Costmdls)
  , ExUnitPrices
  , GenesisDelegateHash(GenesisDelegateHash)
  , GenesisHash(GenesisHash)
  , MIRToStakeCredentials(MIRToStakeCredentials)
  , Mint(Mint)
  , MoveInstantaneousReward(ToOtherPot, ToStakeCreds)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , Redeemer
  , Relay(SingleHostAddr, SingleHostName, MultiHostName)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , URL(URL)
  , UnitInterval
  , Update
  ) as T
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as T
import Ctl.Internal.Cardano.Types.Value as Value
import Ctl.Internal.Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Ctl.Internal.FfiHelpers
  ( ContainerHelper
  , MaybeFfiHelper
  , containerHelper
  , maybeFfiHelper
  )
import Ctl.Internal.Helpers (fromJustEff)
import Ctl.Internal.Serialization.Address
  ( Address
  , RewardAddress
  , StakeCredential
  )
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId, MainnetId)) as T
import Ctl.Internal.Serialization.AuxiliaryData (convertAuxiliaryData)
import Ctl.Internal.Serialization.BigInt as Serialization
import Ctl.Internal.Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , VRFKeyHash
  , scriptHashFromBytes
  )
import Ctl.Internal.Serialization.NativeScript (convertNativeScript)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript)
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Types
  ( AssetName
  , Assets
  , AuxiliaryData
  , AuxiliaryDataHash
  , BigInt
  , Certificate
  , Certificates
  , CostModel
  , Costmdls
  , DataHash
  , Ed25519KeyHashes
  , Ed25519Signature
  , ExUnitPrices
  , ExUnits
  , GenesisDelegateHash
  , GenesisHash
  , Ipv4
  , Ipv6
  , Language
  , MIRToStakeCredentials
  , Mint
  , MintAssets
  , MoveInstantaneousReward
  , MultiAsset
  , NativeScript
  , NetworkId
  , PlutusData
  , PlutusScript
  , PoolMetadata
  , PoolMetadataHash
  , PrivateKey
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey
  , Redeemer
  , Redeemers
  , Relay
  , Relays
  , ScriptDataHash
  , ScriptRef
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionOutput
  , TransactionOutputs
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , UnitInterval
  , Update
  , Value
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  , Withdrawals
  )
import Ctl.Internal.Serialization.WitnessSet
  ( convertExUnits
  , convertRedeemer
  , convertWitnessSet
  )
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (fromBigInt, fromStringUnsafe, toString) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Int as Csl
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PlutusData as PlutusData
import Ctl.Internal.Types.RewardAddress (RewardAddress, unRewardAddress) as T
import Ctl.Internal.Types.Scripts (Language(PlutusV1, PlutusV2)) as S
import Ctl.Internal.Types.TokenName (getTokenName) as TokenName
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput)) as T
import Ctl.Internal.Types.VRFKeyHash (VRFKeyHash(VRFKeyHash), unVRFKeyHash) as T
import Data.Foldable (class Foldable)
import Data.Foldable (null) as Foldable
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for, for_, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Untagged.Union (UndefinedOr, maybeToUor)

foreign import hashTransaction :: TransactionBody -> Effect TransactionHash

foreign import newValue :: BigNum -> Effect Value
foreign import valueSetCoin :: Value -> BigNum -> Effect Unit
foreign import newValueFromAssets :: MultiAsset -> Effect Value
foreign import newTransactionInput
  :: TransactionHash -> UInt -> Effect TransactionInput

foreign import newTransactionInputs :: Effect TransactionInputs
foreign import addTransactionInput
  :: TransactionInputs -> TransactionInput -> Effect Unit

foreign import newTransactionOutput
  :: Address -> Value -> Effect TransactionOutput

foreign import newTransactionOutputs :: Effect TransactionOutputs
foreign import addTransactionOutput
  :: TransactionOutputs -> TransactionOutput -> Effect Unit

foreign import newTransactionBody
  :: TransactionInputs
  -> TransactionOutputs
  -> BigNum
  -> Effect TransactionBody

foreign import newTransaction
  :: TransactionBody
  -> TransactionWitnessSet
  -> AuxiliaryData
  -> Effect Transaction

foreign import newTransaction_
  :: TransactionBody
  -> TransactionWitnessSet
  -> Effect Transaction

foreign import newTransactionUnspentOutput
  :: TransactionInput -> TransactionOutput -> Effect TransactionUnspentOutput

foreign import newMultiAsset :: Effect MultiAsset
foreign import insertMultiAsset
  :: MultiAsset -> ScriptHash -> Assets -> Effect Unit

foreign import newAssets :: Effect Assets
foreign import insertAssets :: Assets -> AssetName -> BigNum -> Effect Unit
foreign import newAssetName :: ByteArray -> Effect AssetName
foreign import transactionOutputSetDataHash
  :: TransactionOutput -> DataHash -> Effect Unit

foreign import transactionOutputSetPlutusData
  :: TransactionOutput -> PlutusData -> Effect Unit

foreign import transactionOutputSetScriptRef
  :: TransactionOutput -> ScriptRef -> Effect Unit

foreign import scriptRefNewNativeScript
  :: NativeScript -> ScriptRef

foreign import scriptRefNewPlutusScript
  :: PlutusScript -> ScriptRef

foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import makeVkeywitness
  :: TransactionHash -> PrivateKey -> Effect Vkeywitness

foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey

foreign import publicKeyHash :: PublicKey -> Ed25519KeyHash

foreign import transactionWitnessSetSetVkeys
  :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit

foreign import defaultCostmdls :: Effect Costmdls
foreign import newCostmdls :: Effect Costmdls
foreign import costmdlsSetCostModel
  :: Costmdls -> Language -> CostModel -> Effect Unit

foreign import newCostModel :: Effect CostModel
foreign import costModelSetCost :: CostModel -> Int -> Csl.Int -> Effect Unit
foreign import newPlutusV1 :: Effect Language
foreign import newPlutusV2 :: Effect Language

foreign import _hashScriptData
  :: Redeemers -> Costmdls -> Array PlutusData -> Effect ScriptDataHash

foreign import _hashScriptDataNoDatums
  :: Redeemers -> Costmdls -> Effect ScriptDataHash

foreign import newRedeemers :: Effect Redeemers
foreign import addRedeemer :: Redeemers -> Redeemer -> Effect Unit
foreign import setTxBodyReferenceInputs
  :: TransactionBody
  -> TransactionInputs
  -> Effect Unit

foreign import setTxBodyScriptDataHash
  :: TransactionBody -> ScriptDataHash -> Effect Unit

foreign import setTxBodyMint :: TransactionBody -> Mint -> Effect Unit
foreign import newMint :: Effect Mint
foreign import newMintAssets :: Effect MintAssets
foreign import _bigIntToInt :: MaybeFfiHelper -> BigInt -> Maybe Int
foreign import insertMintAssets
  :: Mint -> ScriptHash -> MintAssets -> Effect Unit

foreign import insertMintAsset :: MintAssets -> AssetName -> Int -> Effect Unit
foreign import setTxBodyNetworkId :: TransactionBody -> NetworkId -> Effect Unit
foreign import networkIdTestnet :: Effect NetworkId
foreign import networkIdMainnet :: Effect NetworkId

foreign import setTxBodyCollateralReturn
  :: TransactionBody
  -> TransactionOutput
  -> Effect Unit

foreign import setTxBodyTotalCollateral
  :: TransactionBody
  -> BigNum
  -> Effect Unit

foreign import setTxBodyTtl :: TransactionBody -> BigNum -> Effect Unit

foreign import setTxBodyCerts :: TransactionBody -> Certificates -> Effect Unit
foreign import newCertificates :: Effect Certificates
foreign import newStakeRegistrationCertificate
  :: StakeCredential -> Effect Certificate

foreign import newStakeDeregistrationCertificate
  :: StakeCredential -> Effect Certificate

foreign import newStakeDelegationCertificate
  :: StakeCredential -> Ed25519KeyHash -> Effect Certificate

foreign import newPoolRegistrationCertificate
  :: Ed25519KeyHash
  -> VRFKeyHash
  -> BigNum
  -> BigNum
  -> UnitInterval
  -> RewardAddress
  -> Ed25519KeyHashes
  -> Relays
  -> UndefinedOr PoolMetadata
  -> Effect Certificate

foreign import newPoolRetirementCertificate
  :: Ed25519KeyHash -> Int -> Effect Certificate

foreign import newGenesisKeyDelegationCertificate
  :: GenesisHash -> GenesisDelegateHash -> VRFKeyHash -> Effect Certificate

foreign import addCert :: Certificates -> Certificate -> Effect Unit
foreign import newUnitInterval :: BigNum -> BigNum -> Effect UnitInterval
foreign import convertPoolOwners
  :: ContainerHelper -> Array Ed25519KeyHash -> Effect Ed25519KeyHashes

foreign import packRelays :: ContainerHelper -> Array Relay -> Relays
foreign import newIpv4 :: ByteArray -> Effect Ipv4
foreign import newIpv6 :: ByteArray -> Effect Ipv6
foreign import newSingleHostAddr
  :: UndefinedOr Int -> UndefinedOr Ipv4 -> UndefinedOr Ipv6 -> Effect Relay

foreign import newSingleHostName :: UndefinedOr Int -> String -> Effect Relay
foreign import newMultiHostName :: String -> Effect Relay
foreign import newPoolMetadata
  :: String -> PoolMetadataHash -> Effect PoolMetadata

foreign import newMoveInstantaneousRewardToOtherPot
  :: Number -> BigNum -> Effect MoveInstantaneousReward

foreign import newMoveInstantaneousRewardToStakeCreds
  :: Number -> MIRToStakeCredentials -> Effect MoveInstantaneousReward

foreign import newMIRToStakeCredentials
  :: ContainerHelper
  -> Array (StakeCredential /\ Csl.Int)
  -> Effect MIRToStakeCredentials

foreign import newMoveInstantaneousRewardsCertificate
  :: MoveInstantaneousReward -> Effect Certificate

foreign import setTxBodyCollateral
  :: TransactionBody -> TransactionInputs -> Effect Unit

foreign import transactionBodySetRequiredSigners
  :: ContainerHelper -> TransactionBody -> Array Ed25519KeyHash -> Effect Unit

foreign import transactionBodySetValidityStartInterval
  :: TransactionBody -> BigNum -> Effect Unit

foreign import transactionBodySetAuxiliaryDataHash
  :: TransactionBody -> AuxiliaryDataHash -> Effect Unit

foreign import newWithdrawals
  :: ContainerHelper
  -> Array (RewardAddress /\ BigNum)
  -> Effect Withdrawals

foreign import setTxBodyWithdrawals
  :: TransactionBody -> Withdrawals -> Effect Unit

foreign import setTxBodyUpdate
  :: TransactionBody -> Update -> Effect Unit

foreign import newUpdate
  :: ProposedProtocolParameterUpdates -> Int -> Effect Update

foreign import newProtocolParamUpdate :: Effect ProtocolParamUpdate

foreign import ppuSetMinfeeA :: ProtocolParamUpdate -> BigNum -> Effect Unit

foreign import ppuSetMinfeeB :: ProtocolParamUpdate -> BigNum -> Effect Unit

foreign import ppuSetMaxBlockBodySize
  :: ProtocolParamUpdate -> Int -> Effect Unit

foreign import ppuSetMaxTxSize :: ProtocolParamUpdate -> Int -> Effect Unit

foreign import ppuSetMaxBlockHeaderSize
  :: ProtocolParamUpdate -> Int -> Effect Unit

foreign import ppuSetKeyDeposit
  :: ProtocolParamUpdate -> BigNum -> Effect Unit

foreign import ppuSetPoolDeposit
  :: ProtocolParamUpdate -> BigNum -> Effect Unit

foreign import ppuSetMaxEpoch :: ProtocolParamUpdate -> Int -> Effect Unit

foreign import ppuSetNOpt :: ProtocolParamUpdate -> Int -> Effect Unit

foreign import ppuSetPoolPledgeInfluence
  :: ProtocolParamUpdate -> UnitInterval -> Effect Unit

foreign import ppuSetExpansionRate
  :: ProtocolParamUpdate -> UnitInterval -> Effect Unit

foreign import ppuSetTreasuryGrowthRate
  :: ProtocolParamUpdate -> UnitInterval -> Effect Unit

foreign import newProtocolVersion :: Int -> Int -> Effect ProtocolVersion

foreign import ppuSetProtocolVersion
  :: ProtocolParamUpdate
  -> ProtocolVersion
  -> Effect Unit

foreign import ppuSetMinPoolCost
  :: ProtocolParamUpdate
  -> BigNum
  -> Effect Unit

foreign import ppuSetAdaPerUtxoByte
  :: ProtocolParamUpdate
  -> BigNum
  -> Effect Unit

foreign import ppuSetCostModels
  :: ProtocolParamUpdate
  -> Costmdls
  -> Effect Unit

foreign import newExUnitPrices
  :: UnitInterval
  -> UnitInterval
  -> Effect ExUnitPrices

foreign import ppuSetExecutionCosts
  :: ProtocolParamUpdate
  -> ExUnitPrices
  -> Effect Unit

foreign import ppuSetMaxTxExUnits
  :: ProtocolParamUpdate
  -> ExUnits
  -> Effect Unit

foreign import ppuSetMaxBlockExUnits
  :: ProtocolParamUpdate
  -> ExUnits
  -> Effect Unit

foreign import ppuSetMaxValueSize
  :: ProtocolParamUpdate
  -> Int
  -> Effect Unit

foreign import ppuSetCollateralPercentage
  :: ProtocolParamUpdate
  -> Int
  -> Effect Unit

foreign import ppuSetMaxCollateralInputs
  :: ProtocolParamUpdate
  -> Int
  -> Effect Unit

foreign import newProposedProtocolParameterUpdates
  :: ContainerHelper
  -> Array (GenesisHash /\ ProtocolParamUpdate)
  -> Effect ProposedProtocolParameterUpdates

foreign import setTxIsValid :: Transaction -> Boolean -> Effect Unit

convertTxBody :: T.TxBody -> Effect TransactionBody
convertTxBody (T.TxBody body) = do
  inputs <- convertTxInputs body.inputs
  outputs <- convertTxOutputs body.outputs
  fee <- fromJustEff "Failed to convert fee" $ BigNum.fromBigInt
    (unwrap body.fee)
  txBody <- newTransactionBody inputs outputs fee
  for_ body.ttl $ unwrap >>> setTxBodyTtl txBody
  for_ body.certs $ convertCerts >=> setTxBodyCerts txBody
  for_ body.withdrawals $ convertWithdrawals >=> setTxBodyWithdrawals txBody
  for_ body.update $ convertUpdate >=> setTxBodyUpdate txBody
  for_ body.auxiliaryDataHash $
    unwrap >>> wrap >>> fromBytes >>> fromJustEff
      "Failed to convert auxiliary data hash"
      >=> transactionBodySetAuxiliaryDataHash txBody
  for_ body.validityStartInterval
    $ unwrap
        >>> BigNum.toString
        >>> BigNum.fromStringUnsafe
        >>>
          transactionBodySetValidityStartInterval txBody
  for_ body.requiredSigners
    $ map unwrap
        >>> transactionBodySetRequiredSigners containerHelper txBody
  for_ body.networkId $ convertNetworkId >=> setTxBodyNetworkId txBody
  for_ body.mint $ convertMint >=> setTxBodyMint txBody
  for_ body.scriptDataHash $
    unwrap >>> wrap >>> fromBytes >>> fromJustEff
      "Failed to convert script data hash"
      >=> setTxBodyScriptDataHash txBody
  for_ body.collateral $ convertTxInputs >=> setTxBodyCollateral txBody
  for_ body.requiredSigners
    $ map unwrap
        >>> transactionBodySetRequiredSigners containerHelper txBody
  for_ body.networkId $ convertNetworkId >=> setTxBodyNetworkId txBody
  for_ body.collateralReturn $ convertTxOutput >=> setTxBodyCollateralReturn
    txBody
  for_ body.totalCollateral
    $
      unwrap
        >>> BigNum.fromBigInt
        >>> fromJustEff "Failed to convert fee"
        >=>
          setTxBodyTotalCollateral txBody
  if Foldable.null body.referenceInputs then pure unit
  else convertTxInputs body.referenceInputs >>= setTxBodyReferenceInputs txBody
  pure txBody

convertTransaction :: T.Transaction -> Effect Transaction
convertTransaction
  ( T.Transaction
      { body, witnessSet, isValid, auxiliaryData }
  ) =
  do
    txBody <- convertTxBody body
    ws <- convertWitnessSet witnessSet
    mbAuxiliaryData <- for auxiliaryData convertAuxiliaryData
    tx <- case mbAuxiliaryData of
      Nothing -> newTransaction_ txBody ws
      Just ad -> newTransaction txBody ws ad
    setTxIsValid tx isValid
    pure tx

convertUpdate :: T.Update -> Effect Update
convertUpdate { proposedProtocolParameterUpdates, epoch } = do
  ppUpdates <- convertProposedProtocolParameterUpdates
    proposedProtocolParameterUpdates
  newUpdate ppUpdates $ UInt.toInt $ unwrap epoch

convertProposedProtocolParameterUpdates
  :: T.ProposedProtocolParameterUpdates
  -> Effect ProposedProtocolParameterUpdates
convertProposedProtocolParameterUpdates ppus =
  newProposedProtocolParameterUpdates containerHelper =<<
    for (Map.toUnfoldable $ unwrap ppus) \(genesisHash /\ ppu) -> do
      Tuple
        <$>
          ( fromJustEff "Failed to convert genesis hash" $ fromBytes $ wrap
              $ unwrap genesisHash
          )
        <*>
          convertProtocolParamUpdate ppu

convertProtocolParamUpdate
  :: T.ProtocolParamUpdate -> Effect ProtocolParamUpdate
convertProtocolParamUpdate
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
  , expansionRate
  , treasuryGrowthRate
  , protocolVersion
  , minPoolCost
  , adaPerUtxoByte
  , costModels
  , executionCosts
  , maxTxExUnits
  , maxBlockExUnits
  , maxValueSize
  , collateralPercentage
  , maxCollateralInputs
  } = do
  ppu <- newProtocolParamUpdate
  for_ minfeeA $ ppuSetMinfeeA ppu
    <=< fromJustEff "convertProtocolParamUpdate: min_fee_a must not be negative"
      <<< BigNum.fromBigInt
      <<< unwrap
  for_ minfeeB $ ppuSetMinfeeB ppu
    <=< fromJustEff "convertProtocolParamUpdate: min_fee_b must not be negative"
      <<< BigNum.fromBigInt
      <<< unwrap
  for_ maxBlockBodySize $ ppuSetMaxBlockBodySize ppu <<< UInt.toInt
  for_ maxTxSize $ ppuSetMaxTxSize ppu <<< UInt.toInt
  for_ maxBlockHeaderSize $ ppuSetMaxBlockHeaderSize ppu <<< UInt.toInt
  for_ keyDeposit $ ppuSetKeyDeposit ppu
    <=<
      fromJustEff
        "convertProtocolParamUpdate: key_deposit must not be negative"
        <<< BigNum.fromBigInt
        <<< unwrap
  for_ poolDeposit $ ppuSetPoolDeposit ppu
    <=<
      fromJustEff
        "convertProtocolParamUpdate: pool_deposit must not be negative"
        <<< BigNum.fromBigInt
        <<< unwrap
  for_ maxEpoch $ ppuSetMaxEpoch ppu <<< UInt.toInt <<< unwrap
  for_ nOpt $ ppuSetNOpt ppu <<< UInt.toInt
  for_ poolPledgeInfluence
    $ mkUnitInterval
        >=> ppuSetPoolPledgeInfluence ppu
  for_ expansionRate
    $ mkUnitInterval
        >=> ppuSetExpansionRate ppu
  for_ treasuryGrowthRate
    $ mkUnitInterval
        >=> ppuSetTreasuryGrowthRate ppu
  for_ protocolVersion \pv ->
    ppuSetProtocolVersion ppu =<<
      newProtocolVersion (UInt.toInt pv.major)
        (UInt.toInt pv.minor)
  for_ minPoolCost $ ppuSetMinPoolCost ppu
  for_ adaPerUtxoByte $ ppuSetAdaPerUtxoByte ppu
  for_ costModels $ convertCostmdls >=> ppuSetCostModels ppu
  for_ executionCosts $ convertExUnitPrices >=> ppuSetExecutionCosts ppu
  for_ maxTxExUnits $ convertExUnits >=> ppuSetMaxTxExUnits ppu
  for_ maxBlockExUnits $ convertExUnits >=> ppuSetMaxBlockExUnits ppu
  for_ maxValueSize $ UInt.toInt >>> ppuSetMaxValueSize ppu
  for_ collateralPercentage $ UInt.toInt >>> ppuSetCollateralPercentage ppu
  for_ maxCollateralInputs $ UInt.toInt >>> ppuSetMaxCollateralInputs ppu
  pure ppu

mkUnitInterval
  :: T.UnitInterval -> Effect UnitInterval
mkUnitInterval x = newUnitInterval x.numerator x.denominator

convertExUnitPrices
  :: T.ExUnitPrices
  -> Effect ExUnitPrices
convertExUnitPrices { memPrice, stepPrice } =
  join $ newExUnitPrices <$> mkUnitInterval memPrice <*> mkUnitInterval
    stepPrice

convertWithdrawals :: Map.Map T.RewardAddress Value.Coin -> Effect Withdrawals
convertWithdrawals mp =
  newWithdrawals containerHelper =<< do
    for (Map.toUnfoldable mp) \(k /\ Value.Coin v) -> do
      Tuple (T.unRewardAddress k) <$> fromJustEff
        "convertWithdrawals: Failed to convert BigNum"
        (BigNum.fromBigInt v)

convertCerts :: Array T.Certificate -> Effect Certificates
convertCerts certs = do
  certificates <- newCertificates
  for_ certs $ convertCert >=> addCert certificates
  pure certificates

convertCert :: T.Certificate -> Effect Certificate
convertCert = case _ of
  T.StakeRegistration stakeCredential ->
    newStakeRegistrationCertificate stakeCredential
  T.StakeDeregistration stakeCredential ->
    newStakeDeregistrationCertificate stakeCredential
  T.StakeDelegation stakeCredential keyHash ->
    newStakeDelegationCertificate stakeCredential (unwrap $ unwrap keyHash)
  T.PoolRegistration
    { operator
    , vrfKeyhash
    , pledge
    , cost
    , margin
    , rewardAccount
    , poolOwners
    , relays
    , poolMetadata
    } -> do
    margin' <- newUnitInterval margin.numerator margin.denominator
    poolOwners' <- convertPoolOwners containerHelper
      (unwrap <<< unwrap <$> poolOwners)
    relays' <- convertRelays relays
    poolMetadata' <- for poolMetadata convertPoolMetadata
    newPoolRegistrationCertificate (unwrap $ unwrap operator)
      (T.unVRFKeyHash vrfKeyhash)
      pledge
      cost
      margin'
      (T.unRewardAddress rewardAccount)
      poolOwners'
      relays'
      (maybeToUor poolMetadata')
  T.PoolRetirement { poolKeyHash, epoch } ->
    newPoolRetirementCertificate (unwrap $ unwrap poolKeyHash)
      (UInt.toInt $ unwrap epoch)
  T.GenesisKeyDelegation
    { genesisHash: T.GenesisHash genesisHash
    , genesisDelegateHash: T.GenesisDelegateHash genesisDelegateHash
    , vrfKeyhash: T.VRFKeyHash vrfKeyhash
    } -> do
    join $ newGenesisKeyDelegationCertificate
      <$>
        ( fromJustEff "Failed to convert genesis hash"
            $ fromBytes
            $ wrap genesisHash
        )
      <*>
        ( fromJustEff "Failed to convert genesis delegate hash"
            $ fromBytes
            $ wrap genesisDelegateHash
        )
      <*>
        pure vrfKeyhash
  T.MoveInstantaneousRewardsCert mir -> do
    newMoveInstantaneousRewardsCertificate =<<
      convertMoveInstantaneousReward mir

convertMIRToStakeCredentials
  :: T.MIRToStakeCredentials -> Effect MIRToStakeCredentials
convertMIRToStakeCredentials (T.MIRToStakeCredentials mp) =
  newMIRToStakeCredentials containerHelper (Map.toUnfoldable mp)

convertMoveInstantaneousReward
  :: T.MoveInstantaneousReward -> Effect MoveInstantaneousReward
convertMoveInstantaneousReward (T.ToOtherPot { pot, amount }) =
  newMoveInstantaneousRewardToOtherPot pot amount
convertMoveInstantaneousReward (T.ToStakeCreds { pot, amounts }) =
  convertMIRToStakeCredentials amounts >>=
    newMoveInstantaneousRewardToStakeCreds pot

convertPoolMetadata :: T.PoolMetadata -> Effect PoolMetadata
convertPoolMetadata
  (T.PoolMetadata { url: T.URL url, hash: T.PoolMetadataHash hash }) =
  ( fromJustEff "Failed to convert script data hash" <<< fromBytes <<< wrap
      >=> newPoolMetadata url
  ) hash

convertRelays :: Array T.Relay -> Effect Relays
convertRelays relays = do
  packRelays containerHelper <$> for relays \relay -> case relay of
    T.SingleHostAddr { port, ipv4, ipv6 } -> do
      ipv4' <- maybeToUor <$> for (unwrap <$> ipv4) newIpv4
      ipv6' <- maybeToUor <$> for (unwrap <$> ipv6) newIpv6
      newSingleHostAddr (maybeToUor port) ipv4' ipv6'
    T.SingleHostName { port, dnsName } ->
      newSingleHostName (maybeToUor port) dnsName
    T.MultiHostName { dnsName } ->
      newMultiHostName dnsName

convertNetworkId :: T.NetworkId -> Effect NetworkId
convertNetworkId = case _ of
  T.TestnetId -> networkIdTestnet
  T.MainnetId -> networkIdMainnet

convertMint :: T.Mint -> Effect Mint
convertMint (T.Mint nonAdaAssets) = do
  mint <- newMint
  let assetsMap = Value.unwrapNonAdaAsset nonAdaAssets
  forWithIndex_ assetsMap \scriptHashBytes' values -> do
    let
      mScripthash = scriptHashFromBytes $ Value.getCurrencySymbol
        scriptHashBytes'
    scripthash <- fromJustEff
      "scriptHashFromBytes failed while converting value"
      mScripthash
    assets <- newMintAssets
    forWithIndex_ values \tokenName' bigIntValue -> do
      let tokenName = TokenName.getTokenName tokenName'
      assetName <- newAssetName tokenName
      bigInt <- fromJustEff "convertMint: failed to convert BigInt" $
        Serialization.convertBigInt bigIntValue
      int <- fromJustEff "convertMint: numeric overflow or underflow" $
        _bigIntToInt maybeFfiHelper bigInt
      insertMintAsset assets assetName int
    insertMintAssets mint scripthash assets
  pure mint

convertTxInputs
  :: forall (f :: Type -> Type)
   . Foldable f
  => f T.TransactionInput
  -> Effect TransactionInputs
convertTxInputs fInputs = do
  inputs <- newTransactionInputs
  traverse_ (convertTxInput >=> addTransactionInput inputs) fInputs
  pure inputs

convertTxInput :: T.TransactionInput -> Effect TransactionInput
convertTxInput (T.TransactionInput { transactionId, index }) = do
  tx_hash <- fromBytesEffect $ wrap $ unwrap transactionId
  newTransactionInput tx_hash index

convertTxOutputs :: Array T.TransactionOutput -> Effect TransactionOutputs
convertTxOutputs arrOutputs = do
  outputs <- newTransactionOutputs
  traverse_ (convertTxOutput >=> addTransactionOutput outputs) arrOutputs
  pure outputs

convertTxOutput :: T.TransactionOutput -> Effect TransactionOutput
convertTxOutput
  (T.TransactionOutput { address, amount, datum, scriptRef }) = do
  value <- convertValue amount
  txo <- newTransactionOutput address value
  case datum of
    NoOutputDatum -> pure unit
    OutputDatumHash dataHash -> do
      for_ (fromBytes $ wrap $ unwrap dataHash) $
        transactionOutputSetDataHash txo
    OutputDatum datumValue -> do
      transactionOutputSetPlutusData txo
        $ convertPlutusData
        $ unwrap datumValue
  for_ scriptRef $
    convertScriptRef >>> transactionOutputSetScriptRef txo
  pure txo

convertScriptRef :: T.ScriptRef -> ScriptRef
convertScriptRef (T.NativeScriptRef nativeScript) =
  scriptRefNewNativeScript $ convertNativeScript nativeScript
convertScriptRef (T.PlutusScriptRef plutusScript) =
  scriptRefNewPlutusScript $ convertPlutusScript plutusScript

convertValue :: Value.Value -> Effect Value
convertValue val = do
  let
    lovelace = Value.valueToCoin' val
    m = Value.getNonAdaAsset' val
  multiasset <- newMultiAsset
  forWithIndex_ m \scriptHashBytes' values -> do
    let
      mScripthash = scriptHashFromBytes $ Value.getCurrencySymbol
        scriptHashBytes'
    scripthash <- fromJustEff
      "scriptHashFromBytes failed while converting value"
      mScripthash
    assets <- newAssets
    forWithIndex_ values \tokenName' bigIntValue -> do
      let tokenName = TokenName.getTokenName tokenName'
      assetName <- newAssetName tokenName
      value <- fromJustEff "convertValue: number must not be negative" $
        BigNum.fromBigInt bigIntValue
      insertAssets assets assetName value
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< fromJustEff
    "convertValue: coin value must not be negative"
    (BigNum.fromBigInt lovelace)
  pure value

convertCostmdls :: T.Costmdls -> Effect Costmdls
convertCostmdls (T.Costmdls cs) = do
  costmdls <- newCostmdls
  forWithIndex_ cs \language costModel -> do
    language' <- case language of
      S.PlutusV1 -> newPlutusV1
      S.PlutusV2 -> newPlutusV2
    costModel' <- convertCostModel costModel
    costmdlsSetCostModel costmdls language' costModel'
  pure costmdls

convertCostModel :: T.CostModel -> Effect CostModel
convertCostModel (T.CostModel costs) = do
  costModel <- newCostModel
  forWithIndex_ costs $ \operation cost ->
    costModelSetCost costModel operation cost
  pure costModel

convertTransactionUnspentOutput
  :: T.TransactionUnspentOutput -> Effect TransactionUnspentOutput
convertTransactionUnspentOutput (T.TransactionUnspentOutput { input, output }) =
  do
    input' <- convertTxInput input
    output' <- convertTxOutput output
    newTransactionUnspentOutput input' output'

hashScriptData
  :: T.Costmdls
  -> Array T.Redeemer
  -> Array PlutusData.PlutusData
  -> Effect ScriptDataHash
hashScriptData cms rs ps = do
  rs' <- newRedeemers
  cms' <- convertCostmdls cms
  traverse_ (addRedeemer rs' <=< convertRedeemer) rs
  -- If an empty `PlutusData` array is passed to CSL's script integrity hashing
  -- function, the resulting hash will be wrong
  case ps of
    [] -> _hashScriptDataNoDatums rs' cms'
    _ -> _hashScriptData rs' cms' $ map convertPlutusData ps

serializeData :: forall (a :: Type). ToData a => a -> CborBytes
serializeData = toBytes <<< convertPlutusData <<< toData
