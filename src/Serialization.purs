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

import Data.BigInt as BigInt
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_, for)
import Data.Tuple.Nested (type (/\))
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
  , BigNum
  , BigInt
  , Certificates
  , Certificate
  , Costmdls
  , CostModel
  , DataHash
  , Ed25519Signature
  , Int32
  , Language
  , Mint
  , MintAssets
  , MultiAsset
  , NativeScript
  , NetworkId
  , PlutusData
  , PlutusList
  , PlutusScripts
  , PublicKey
  , Redeemer
  , Redeemers
  , ScriptDataHash
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionOutput
  , TransactionOutputs
  , TransactionWitnessSet
  , Value
  , Vkey
  , Vkeywitnesses
  , PlutusScript
  , Vkeywitness
  , UnitInterval
  , Ed25519KeyHashes
  , Relay
  , Relays
  , Ipv4
  , Ipv6
  , PoolMetadata
  , VRFKeyHash
  , GenesisHash
  , GenesisDelegateHash
  , MoveInstantaneousReward
  , MIRToStakeCredentials
  )
import Serialization.WitnessSet (convertWitnessSet, convertRedeemer)
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
  , Language(PlutusV1)
  , Mint(Mint)
  , Redeemer
  , Transaction(Transaction)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Relay(SingleHostAddr, SingleHostName, MultiHostName)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , URL(URL)
  , GenesisHash(GenesisHash)
  , GenesisDelegateHash(GenesisDelegateHash)
  , MoveInstantaneousReward(ToOtherPot, ToStakeCreds)
  , MIRToStakeCredentials(MIRToStakeCredentials)
  ) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.Value as Value
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
  for_ body.mint $ convertMint >=> setTxBodyMint txBody
  for_ body.certs $ convertCerts >=> setTxBodyCerts txBody
  for_ body.collateral $ convertTxInputs >=> setTxBodyCollateral txBody
  ws <- convertWitnessSet witnessSet
  newTransaction txBody ws

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
      let tokenName = Value.getTokenName tokenName'
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
      let tokenName = Value.getTokenName tokenName'
      assetName <- newAssetName tokenName
      value <- fromJustEff "convertValue: number must not be negative" $
        newBigNum maybeFfiHelper (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< fromJustEff
    "convertValue: coin value must not be negative"
    (newBigNum maybeFfiHelper (BigInt.toString lovelace))
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
