module Serialization
  ( convertTransaction
  , convertTxInput
  , convertTxOutput
  , toBytes
  , newTransactionUnspentOutputFromBytes
  , newTransactionWitnessSetFromBytes
  , hashScriptData
  , publicKeyHash
  , publicKeyFromBech32
  ) where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_, for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Effect (Effect)
import FfiHelpers
  ( MaybeFfiHelper
  , maybeFfiHelper
  , ContainerHelper
  , containerHelper
  )
import Helpers (fromJustEff)
import Serialization.Address (Address, StakeCredential, RewardAddress)
import Serialization.Address (NetworkId(TestnetId, MainnetId)) as T
import Serialization.BigInt as Serialization
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.Hash (ScriptHash, Ed25519KeyHash, scriptHashFromBytes)
import Serialization.PlutusData (packPlutusList)
import Serialization.Types
  ( AssetName
  , Assets
  , AuxiliaryData
  , BigInt
  , BigNum
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
  , Int32
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
  , PlutusList
  , PlutusScript
  , PlutusScripts
  , PoolMetadata
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey
  , Redeemer
  , Redeemers
  , Relay
  , Relays
  , ScriptDataHash
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionOutput
  , TransactionOutputs
  , TransactionWitnessSet
  , UnitInterval
  , Update
  , VRFKeyHash
  , Value
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  , Withdrawals
  )
import Serialization.WitnessSet
  ( convertWitnessSet
  , convertRedeemer
  , convertExUnits
  )
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.Int as Int
import Types.PlutusData as PlutusData
import Types.Transaction
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , GenesisKeyDelegation
      , MoveInstantaneousRewardsCert
      )
  , Costmdls(Costmdls)
  , GenesisDelegateHash(GenesisDelegateHash)
  , GenesisHash(GenesisHash)
  , Language(PlutusV1)
  , MIRToStakeCredentials(MIRToStakeCredentials)
  , Mint(Mint)
  , MoveInstantaneousReward(ToOtherPot, ToStakeCreds)
  , Nonce(IdentityNonce, HashNonce)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , Redeemer
  , Relay(SingleHostAddr, SingleHostName, MultiHostName)
  , Transaction(Transaction)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , UnitInterval
  , URL(URL)
  , Update
  ) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.Value as Value
import Types.TokenName (getTokenName) as TokenName
import Untagged.Union (type (|+|), UndefinedOr, maybeToUor)

foreign import newBigNum :: MaybeFfiHelper -> String -> Maybe BigNum
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
  -> UndefinedOr Int
  -> Effect TransactionBody

foreign import newTransaction
  :: TransactionBody -> TransactionWitnessSet -> Effect Transaction

foreign import newTransaction_
  :: TransactionBody
  -> TransactionWitnessSet
  -> AuxiliaryData
  -> Effect Transaction

foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newTransactionWitnessSetFromBytes
  :: ByteArray -> Effect TransactionWitnessSet

foreign import newTransactionUnspentOutputFromBytes
  :: ByteArray -> Effect TransactionUnspentOutput

foreign import newMultiAsset :: Effect MultiAsset
foreign import insertMultiAsset
  :: MultiAsset -> ScriptHash -> Assets -> Effect Unit

foreign import newAssets :: Effect Assets
foreign import insertAssets :: Assets -> AssetName -> BigNum -> Effect Unit
foreign import newAssetName :: ByteArray -> Effect AssetName
foreign import transactionOutputSetDataHash
  :: TransactionOutput -> DataHash -> Effect Unit

foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey
foreign import _publicKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PublicKey

foreign import publicKeyHash :: PublicKey -> Ed25519KeyHash
foreign import newEd25519Signature :: Bech32String -> Effect Ed25519Signature
foreign import transactionWitnessSetSetVkeys
  :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit

foreign import newPlutusScript :: ByteArray -> Effect PlutusScript
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import txWitnessSetSetPlutusScripts
  :: TransactionWitnessSet -> PlutusScripts -> Effect Unit

foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit
foreign import newCostmdls :: Effect Costmdls
foreign import costmdlsSetCostModel
  :: Costmdls -> Language -> CostModel -> Effect Unit

foreign import newCostModel :: Effect CostModel
foreign import costModelSetCost :: CostModel -> Int -> Int32 -> Effect Unit
foreign import newPlutusV1 :: Effect Language
foreign import newInt32 :: Int -> Effect Int32
foreign import _hashScriptData
  :: Redeemers -> Costmdls -> PlutusList -> Effect ScriptDataHash

foreign import newRedeemers :: Effect Redeemers
foreign import addRedeemer :: Redeemers -> Redeemer -> Effect Unit
foreign import newScriptDataHashFromBytes :: ByteArray -> Effect ScriptDataHash
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
foreign import newPoolMetadata :: String -> ByteArray -> Effect PoolMetadata
foreign import newGenesisHash :: ByteArray -> Effect GenesisHash
foreign import newGenesisDelegateHash :: ByteArray -> Effect GenesisDelegateHash
foreign import newMoveInstantaneousRewardToOtherPot
  :: Number -> BigNum -> Effect MoveInstantaneousReward

foreign import newMoveInstantaneousRewardToStakeCreds
  :: Number -> MIRToStakeCredentials -> Effect MoveInstantaneousReward

foreign import newMIRToStakeCredentials
  :: ContainerHelper
  -> Array (StakeCredential /\ Int.Int)
  -> Effect MIRToStakeCredentials

foreign import newMoveInstantaneousRewardsCertificate
  :: MoveInstantaneousReward -> Effect Certificate

foreign import setTxBodyCollateral
  :: TransactionBody -> TransactionInputs -> Effect Unit

foreign import transactionBodySetRequiredSigners
  :: ContainerHelper -> TransactionBody -> Array Ed25519KeyHash -> Effect Unit

foreign import transactionBodySetValidityStartInterval
  :: TransactionBody -> Int -> Effect Unit

foreign import transactionBodySetAuxiliaryDataHash
  :: TransactionBody -> ByteArray -> Effect Unit

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

foreign import ppuSetD :: ProtocolParamUpdate -> UnitInterval -> Effect Unit

foreign import ppuSetExtraEntropyIdentity
  :: ProtocolParamUpdate -> Effect Unit

foreign import ppuSetExtraEntropyFromHash
  :: ProtocolParamUpdate -> ByteArray -> Effect Unit

foreign import newProtocolVersion :: Int -> Int -> Effect ProtocolVersion

foreign import ppuSetProtocolVersion
  :: ContainerHelper
  -> ProtocolParamUpdate
  -> Array ProtocolVersion
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

foreign import newProposedProtocolParameterUpdates
  :: ContainerHelper
  -> Array (GenesisHash /\ ProtocolParamUpdate)
  -> Effect ProposedProtocolParameterUpdates

-- NOTE returns cbor encoding for all but hash types, for which it returns raw bytes
foreign import toBytes
  :: ( Transaction
         |+| TransactionOutput
         |+| TransactionHash
         |+| DataHash
         |+| PlutusData
         |+| TransactionWitnessSet
         |+| NativeScript
         |+| ScriptDataHash
         |+| Redeemers
     -- Add more as needed.
     )
  -> ByteArray

convertTransaction :: T.Transaction -> Effect Transaction
convertTransaction (T.Transaction { body: T.TxBody body, witnessSet }) = do
  inputs <- convertTxInputs body.inputs
  outputs <- convertTxOutputs body.outputs
  fee <- fromJustEff "Failed to convert fee" $ bigNumFromBigInt
    (unwrap body.fee)
  let ttl = body.ttl <#> unwrap >>> UInt.toInt
  txBody <- newTransactionBody inputs outputs fee (maybeToUor ttl)
  for_ body.validityStartInterval $
    unwrap >>> UInt.toInt >>> transactionBodySetValidityStartInterval txBody
  for_ body.requiredSigners $
    map unwrap >>> transactionBodySetRequiredSigners containerHelper txBody
  for_ body.auxiliaryDataHash $
    unwrap >>> transactionBodySetAuxiliaryDataHash txBody
  for_ body.networkId $ convertNetworkId >=> setTxBodyNetworkId txBody
  traverse_
    (unwrap >>> newScriptDataHashFromBytes >=> setTxBodyScriptDataHash txBody)
    body.scriptDataHash
  for_ body.withdrawals $ convertWithdrawals >=> setTxBodyWithdrawals txBody
  for_ body.mint $ convertMint >=> setTxBodyMint txBody
  for_ body.certs $ convertCerts >=> setTxBodyCerts txBody
  for_ body.collateral $ convertTxInputs >=> setTxBodyCollateral txBody
  for_ body.update $ convertUpdate >=> setTxBodyUpdate txBody
  ws <- convertWitnessSet witnessSet
  newTransaction txBody ws

convertUpdate :: T.Update -> Effect Update
convertUpdate { proposedProtocolParameterUpdates, epoch } = do
  ppUpdates <- convertProposedProtocolParameterUpdates
    proposedProtocolParameterUpdates
  newUpdate ppUpdates (UInt.toInt $ unwrap epoch)

convertProposedProtocolParameterUpdates
  :: T.ProposedProtocolParameterUpdates
  -> Effect ProposedProtocolParameterUpdates
convertProposedProtocolParameterUpdates ppus =
  newProposedProtocolParameterUpdates containerHelper =<<
    for (Map.toUnfoldable $ unwrap ppus) \(genesisHash /\ ppu) -> do
      Tuple <$> newGenesisHash (unwrap genesisHash) <*>
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
  , d
  , extraEntropy
  , protocolVersion
  , minPoolCost
  , adaPerUtxoByte
  , costModels
  , executionCosts
  , maxTxExUnits
  , maxBlockExUnits
  , maxValueSize
  } = do
  ppu <- newProtocolParamUpdate
  for_ minfeeA $ ppuSetMinfeeA ppu <=<
    fromJustEff "convertProtocolParamUpdate: min_fee_a must not be negative"
      <<< bigNumFromBigInt
      <<< unwrap
  for_ minfeeB $ ppuSetMinfeeB ppu <=<
    fromJustEff "convertProtocolParamUpdate: min_fee_b must not be negative"
      <<< bigNumFromBigInt
      <<< unwrap
  for_ maxBlockBodySize $ ppuSetMaxBlockBodySize ppu <<< UInt.toInt
  for_ maxTxSize $ ppuSetMaxTxSize ppu <<< UInt.toInt
  for_ maxBlockHeaderSize $ ppuSetMaxBlockHeaderSize ppu <<< UInt.toInt
  for_ keyDeposit $ ppuSetKeyDeposit ppu <=<
    fromJustEff "convertProtocolParamUpdate: key_deposit must not be negative"
      <<< bigNumFromBigInt
      <<< unwrap
  for_ poolDeposit $ ppuSetPoolDeposit ppu <=<
    fromJustEff "convertProtocolParamUpdate: pool_deposit must not be negative"
      <<< bigNumFromBigInt
      <<< unwrap
  for_ maxEpoch $ ppuSetMaxEpoch ppu <<< UInt.toInt <<< unwrap
  for_ nOpt $ ppuSetNOpt ppu <<< UInt.toInt
  for_ poolPledgeInfluence $
    mkUnitInterval >=> ppuSetPoolPledgeInfluence ppu
  for_ expansionRate $
    mkUnitInterval >=> ppuSetExpansionRate ppu
  for_ treasuryGrowthRate $
    mkUnitInterval >=> ppuSetTreasuryGrowthRate ppu
  for_ d $
    mkUnitInterval >=> ppuSetD ppu
  for_ extraEntropy $ case _ of
    T.IdentityNonce -> ppuSetExtraEntropyIdentity ppu
    T.HashNonce bytes -> ppuSetExtraEntropyFromHash ppu bytes
  for_ protocolVersion $
    ppuSetProtocolVersion containerHelper ppu <=<
      traverse \pv -> newProtocolVersion (UInt.toInt pv.major)
        (UInt.toInt pv.minor)
  for_ minPoolCost $ ppuSetMinPoolCost ppu
  for_ adaPerUtxoByte $ ppuSetAdaPerUtxoByte ppu
  for_ costModels $ convertCostmdls >=> ppuSetCostModels ppu
  for_ executionCosts $ convertExUnitPrices >=> ppuSetExecutionCosts ppu
  for_ maxTxExUnits $ convertExUnits >=> ppuSetMaxTxExUnits ppu
  for_ maxBlockExUnits $ convertExUnits >=> ppuSetMaxBlockExUnits ppu
  for_ maxValueSize $ UInt.toInt >>> ppuSetMaxValueSize ppu
  pure ppu
  where
  mkUnitInterval
    :: T.UnitInterval -> Effect UnitInterval
  mkUnitInterval x = newUnitInterval x.numerator x.denominator

  convertExUnitPrices
    :: { memPrice :: T.UnitInterval, stepPrice :: T.UnitInterval }
    -> Effect ExUnitPrices
  convertExUnitPrices { memPrice, stepPrice } =
    join $ newExUnitPrices <$> mkUnitInterval memPrice <*> mkUnitInterval
      stepPrice

convertWithdrawals :: Map.Map RewardAddress Value.Coin -> Effect Withdrawals
convertWithdrawals mp =
  newWithdrawals containerHelper =<< do
    for (Map.toUnfoldable mp) \(k /\ Value.Coin v) -> do
      Tuple k <$> fromJustEff "convertWithdrawals: Failed to convert BigNum"
        (bigNumFromBigInt v)

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = _publicKeyFromBech32 maybeFfiHelper

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
    newStakeDelegationCertificate stakeCredential keyHash
  T.PoolRegistration
    { operator
    , vrfKeyhash
    , pledge
    , cost
    , margin
    , reward_account
    , poolOwners
    , relays
    , poolMetadata
    } -> do
    margin' <- newUnitInterval margin.numerator margin.denominator
    poolOwners' <- convertPoolOwners containerHelper poolOwners
    relays' <- convertRelays relays
    poolMetadata' <- for poolMetadata convertPoolMetadata
    newPoolRegistrationCertificate operator vrfKeyhash pledge cost margin'
      reward_account
      poolOwners'
      relays'
      (maybeToUor poolMetadata')
  T.PoolRetirement { poolKeyhash, epoch } ->
    newPoolRetirementCertificate poolKeyhash (UInt.toInt $ unwrap epoch)
  T.GenesisKeyDelegation
    { genesisHash: T.GenesisHash genesisHash
    , genesisDelegateHash: T.GenesisDelegateHash genesisDelegateHash
    , vrfKeyhash
    } -> do
    join $ newGenesisKeyDelegationCertificate
      <$> newGenesisHash genesisHash
      <*> newGenesisDelegateHash genesisDelegateHash
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
  newPoolMetadata url hash

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
convertMint (T.Mint (Value.NonAdaAsset m)) = do
  mint <- newMint
  forWithIndex_ m \scriptHashBytes' values -> do
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
      int <- fromJustEff "converMint: numeric overflow or underflow" $
        _bigIntToInt maybeFfiHelper bigInt
      insertMintAsset assets assetName int
    insertMintAssets mint scripthash assets
  pure mint

convertTxInputs :: Array T.TransactionInput -> Effect TransactionInputs
convertTxInputs arrInputs = do
  inputs <- newTransactionInputs
  traverse_ (convertTxInput >=> addTransactionInput inputs) arrInputs
  pure inputs

convertTxInput :: T.TransactionInput -> Effect TransactionInput
convertTxInput (T.TransactionInput { transactionId, index }) = do
  tx_hash <- fromBytesEffect (unwrap transactionId)
  newTransactionInput tx_hash index

convertTxOutputs :: Array T.TransactionOutput -> Effect TransactionOutputs
convertTxOutputs arrOutputs = do
  outputs <- newTransactionOutputs
  traverse_ (convertTxOutput >=> addTransactionOutput outputs) arrOutputs
  pure outputs

convertTxOutput :: T.TransactionOutput -> Effect TransactionOutput
convertTxOutput (T.TransactionOutput { address, amount, dataHash }) = do
  value <- convertValue amount
  txo <- newTransactionOutput address value
  for_ (unwrap <$> dataHash) \bytes -> do
    for_ (fromBytes bytes) $
      transactionOutputSetDataHash txo
  pure txo

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
        bigNumFromBigInt bigIntValue
      insertAssets assets assetName value
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< fromJustEff
    "convertValue: coin value must not be negative"
    (bigNumFromBigInt lovelace)
  pure value

convertCostmdls :: T.Costmdls -> Effect Costmdls
convertCostmdls (T.Costmdls cs) = do
  costs <- map unwrap <<< fromJustEff "`PlutusV1` not found in `Costmdls`"
    $ Map.lookup T.PlutusV1 cs
  costModel <- newCostModel
  forWithIndex_ costs $ \operation cost ->
    costModelSetCost costModel operation =<< newInt32 (UInt.toInt cost)
  costmdls <- newCostmdls
  plutusV1 <- newPlutusV1
  costmdlsSetCostModel costmdls plutusV1 costModel
  pure costmdls

hashScriptData
  :: Array T.Redeemer
  -> T.Costmdls
  -> Array PlutusData.PlutusData
  -> Effect ScriptDataHash
hashScriptData rs cms ps = do
  plist <- fromJustEff "failed to convert datums" $ packPlutusList ps
  rs' <- newRedeemers
  cms' <- convertCostmdls cms
  traverse_ (addRedeemer rs' <=< convertRedeemer) rs
  _hashScriptData rs' cms' plist
