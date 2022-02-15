module Serialization
  ( convertTransaction
  , convertBigInt
  , convertTxInput
  , convertTxOutput
  , newAssetName
  , toBytes
  , newTransactionUnspentOutputFromBytes
  , newTransactionWitnessSetFromBytes
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_)
import Data.UInt (UInt)
import Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Effect (Effect)
import Helpers (errorOnNothing)
import Serialization.Address (Address)
import Serialization.Hash (Ed25519KeyHash, ScriptHash, scriptHashFromBytes)
import Serialization.Types (AssetName, Assets, AuxiliaryData, BigNum, DataHash, Ed25519Signature, MultiAsset, PlutusData, PlutusScript, PlutusScripts, PublicKey, Transaction, TransactionBody, TransactionHash, TransactionInput, TransactionInputs, TransactionOutput, TransactionOutputs, TransactionUnspentOutput, TransactionWitnessSet, Value, Vkey, Vkeywitness, Vkeywitnesses)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.Transaction as T
import Types.Value as Value
import Untagged.Union (type (|+|))

foreign import newBigNum :: String -> Effect BigNum
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

foreign import toBytes
  :: ( Transaction
         |+| TransactionOutput
         |+| Ed25519KeyHash
         |+| ScriptHash
         |+| TransactionHash
         |+| DataHash
         |+| PlutusData
     -- Add more as needed.
     )
  -> ByteArray

convertTransaction :: T.Transaction -> Effect Transaction
convertTransaction (T.Transaction { body: T.TxBody body, witness_set }) = do
  inputs <- convertTxInputs body.inputs
  outputs <- convertTxOutputs body.outputs
  fee <- convertBigInt (unwrap body.fee)
  txBody <- newTransactionBody inputs outputs fee
  ws <- convertWitnessSet witness_set
  newTransaction txBody ws

convertBigInt :: BigInt.BigInt -> Effect BigNum
convertBigInt = newBigNum <<< BigInt.toString

-- Parts that are commented-out are unused & untested, but should compile

convertWitnessSet :: T.TransactionWitnessSet -> Effect TransactionWitnessSet
convertWitnessSet (T.TransactionWitnessSet _ {- tws -} ) = do
  ws <- newTransactionWitnessSet
  -- for_ tws.vkeys
  --   (convertVkeywitnesses >=> transactionWitnessSetSetVkeys ws)
  -- for_ tws.plutus_scripts \ps -> do
  --   scripts <- newPlutusScripts
  --   for_ ps (convertPlutusScript >=> addPlutusScript scripts)
  --   txWitnessSetSetPlutusScripts ws scripts
  pure ws

-- convertPlutusScript :: T.PlutusScript -> Effect PlutusScript
-- convertPlutusScript (T.PlutusScript bytes) = do
--   newPlutusScript bytes

-- convertVkeywitnesses :: Array T.Vkeywitness -> Effect Vkeywitnesses
-- convertVkeywitnesses arr = do
--   witnesses <- newVkeywitnesses
--   traverse_ (convertVkeywitness >=> addVkeywitness witnesses) arr
--   pure witnesses

-- convertVkeywitness :: T.Vkeywitness -> Effect Vkeywitness
-- convertVkeywitness (T.Vkeywitness (vkey /\ signature)) = do
--   vkey' <- convertVkey vkey
--   signature' <- convertEd25519Signature signature
--   newVkeywitness vkey' signature'

-- convertEd25519Signature :: T.Ed25519Signature -> Effect Ed25519Signature
-- convertEd25519Signature (T.Ed25519Signature bech32) =
--   newEd25519Signature bech32

-- convertVkey :: T.Vkey -> Effect Vkey
-- convertVkey (T.Vkey (T.PublicKey pk)) =
--   newPublicKey pk >>= newVkeyFromPublicKey

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
    assets <- newAssets

    scripthash <- errorOnNothing mScripthash

    forWithIndex_ values \tokenName' bigIntValue -> do
      let tokenName = Value.getTokenName tokenName'
      assetName <- newAssetName tokenName
      value <- newBigNum (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< newBigNum (BigInt.toString lovelace)
  pure value
