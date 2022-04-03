module Serialization
  ( convertTransaction
  , convertTxInput
  , convertTxOutput
  , toBytes
  , newTransactionUnspentOutputFromBytes
  , newTransactionWitnessSetFromBytes
  , hashScriptData
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_)
import Data.UInt (UInt)
import Data.UInt as UInt
import Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Effect (Effect)
import Effect.Exception (throw)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Helpers (fromJustEff)
import Serialization.Address (Address)
import Serialization.Address (NetworkId(TestnetId, MainnetId)) as T
import Serialization.BigInt as Serialization
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Serialization.PlutusData (packPlutusList)
import Serialization.Types
  ( AssetName
  , Assets
  , AuxiliaryData
  , BigNum
  , BigInt
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
  )
import Serialization.WitnessSet (convertWitnessSet, convertRedeemer)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.PlutusData as PlutusData
import Types.Transaction
  ( Costmdls(Costmdls)
  , Language(PlutusV1)
  , Mint(Mint)
  , Redeemer
  , Transaction(Transaction)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  ) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.Value as Value
import Untagged.Union (type (|+|))

foreign import newBigNum :: MaybeFfiHelper -> String -> Maybe BigNum
foreign import newValue :: BigNum -> Effect Value
foreign import valueSetCoin :: Value -> BigNum -> Effect Unit
foreign import newValueFromAssets :: MultiAsset -> Effect Value
foreign import newTransactionInput :: TransactionHash -> UInt -> Effect TransactionInput
foreign import newTransactionInputs :: Effect TransactionInputs
foreign import addTransactionInput :: TransactionInputs -> TransactionInput -> Effect Unit
foreign import newTransactionOutput :: Address -> Value -> Effect TransactionOutput
foreign import newTransactionOutputs :: Effect TransactionOutputs
foreign import addTransactionOutput :: TransactionOutputs -> TransactionOutput -> Effect Unit
foreign import newTransactionBody :: TransactionInputs -> TransactionOutputs -> BigNum -> Effect TransactionBody
foreign import newTransaction :: TransactionBody -> TransactionWitnessSet -> Effect Transaction
foreign import newTransaction_ :: TransactionBody -> TransactionWitnessSet -> AuxiliaryData -> Effect Transaction
foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newTransactionWitnessSetFromBytes :: ByteArray -> Effect TransactionWitnessSet
foreign import newTransactionUnspentOutputFromBytes :: ByteArray -> Effect TransactionUnspentOutput
foreign import newMultiAsset :: Effect MultiAsset
foreign import insertMultiAsset :: MultiAsset -> ScriptHash -> Assets -> Effect Unit
foreign import newAssets :: Effect Assets
foreign import insertAssets :: Assets -> AssetName -> BigNum -> Effect Unit
foreign import newAssetName :: ByteArray -> Effect AssetName
foreign import transactionOutputSetDataHash :: TransactionOutput -> DataHash -> Effect Unit
foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey
foreign import newPublicKey :: Bech32String -> Effect PublicKey
foreign import newEd25519Signature :: Bech32String -> Effect Ed25519Signature
foreign import transactionWitnessSetSetVkeys :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit
foreign import newPlutusScript :: ByteArray -> Effect PlutusScript
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import txWitnessSetSetPlutusScripts :: TransactionWitnessSet -> PlutusScripts -> Effect Unit
foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit
foreign import newCostmdls :: Effect Costmdls
foreign import costmdlsSetCostModel :: Costmdls -> Language -> CostModel -> Effect Unit
foreign import newCostModel :: Effect CostModel
foreign import costModelSetCost :: CostModel -> Int -> Int32 -> Effect Unit
foreign import newPlutusV1 :: Effect Language
foreign import newInt32 :: Int -> Effect Int32
foreign import _hashScriptData :: Redeemers -> Costmdls -> PlutusList -> Effect ScriptDataHash
foreign import newRedeemers :: Effect Redeemers
foreign import addRedeemer :: Redeemers -> Redeemer -> Effect Unit
foreign import newScriptDataHashFromBytes :: ByteArray -> Effect ScriptDataHash
foreign import setTxBodyScriptDataHash :: TransactionBody -> ScriptDataHash -> Effect Unit
foreign import setTxBodyMint :: TransactionBody -> Mint -> Effect Unit
foreign import newMint :: Effect Mint
foreign import newMintAssets :: Effect MintAssets
foreign import _bigIntToInt :: MaybeFfiHelper -> BigInt -> Maybe Int
foreign import insertMintAssets :: Mint -> ScriptHash -> MintAssets -> Effect Unit
foreign import insertMintAsset :: MintAssets -> AssetName -> Int -> Effect Unit
foreign import setTxBodyNetworkId :: TransactionBody -> NetworkId -> Effect Unit
foreign import networkIdTestnet :: Effect NetworkId
foreign import networkIdMainnet :: Effect NetworkId
foreign import setTxBodyCollateral :: TransactionBody -> TransactionInputs -> Effect Unit

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
convertTransaction (T.Transaction { body: T.TxBody body, witness_set }) = do
  inputs <- convertTxInputs body.inputs
  outputs <- convertTxOutputs body.outputs
  fee <- maybe (throw "Failed to convert fee") pure $ bigNumFromBigInt (unwrap body.fee)
  txBody <- newTransactionBody inputs outputs fee
  for_ body.network_id $ convertNetworkId >=> setTxBodyNetworkId txBody
  traverse_
    (unwrap >>> newScriptDataHashFromBytes >=> setTxBodyScriptDataHash txBody)
    body.script_data_hash
  for_ body.mint $ convertMint >=> setTxBodyMint txBody
  for_ body.collateral $ convertTxInputs >=> setTxBodyCollateral txBody
  ws <- convertWitnessSet witness_set
  newTransaction txBody ws

convertNetworkId :: T.NetworkId -> Effect NetworkId
convertNetworkId = case _ of
  T.TestnetId -> networkIdTestnet
  T.MainnetId -> networkIdMainnet

convertMint :: T.Mint -> Effect Mint
convertMint (T.Mint (Value.NonAdaAsset m)) = do
  mint <- newMint
  forWithIndex_ m \scriptHashBytes' values -> do
    let mScripthash = scriptHashFromBytes $ Value.getCurrencySymbol scriptHashBytes'
    scripthash <- fromJustEff "scriptHashFromBytes failed while converting value" mScripthash
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
convertTxInput (T.TransactionInput { transaction_id, index }) = do
  tx_hash <- fromBytesEffect (unwrap transaction_id)
  newTransactionInput tx_hash index

convertTxOutputs :: Array T.TransactionOutput -> Effect TransactionOutputs
convertTxOutputs arrOutputs = do
  outputs <- newTransactionOutputs
  traverse_ (convertTxOutput >=> addTransactionOutput outputs) arrOutputs
  pure outputs

convertTxOutput :: T.TransactionOutput -> Effect TransactionOutput
convertTxOutput (T.TransactionOutput { address, amount, data_hash }) = do
  value <- convertValue amount
  txo <- newTransactionOutput address value
  for_ (unwrap <$> data_hash) \bytes -> do
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
    let mScripthash = scriptHashFromBytes $ Value.getCurrencySymbol scriptHashBytes'
    scripthash <- fromJustEff "scriptHashFromBytes failed while converting value" mScripthash
    assets <- newAssets
    forWithIndex_ values \tokenName' bigIntValue -> do
      let tokenName = Value.getTokenName tokenName'
      assetName <- newAssetName tokenName
      -- possible failure below, because bigIntValue can be negative
      value <- fromJustEff "convertValue: number must not be negative" $
        newBigNum maybeFfiHelper (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< fromJustEff "convertValue: coin value must not be negative"
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
