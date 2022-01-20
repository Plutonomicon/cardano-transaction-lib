module Serialize where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt as BigInt
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Prelude
import Types.Transaction as T

-- * cardano-serialization-lib types

foreign import data BigNum :: Type
foreign import data Value :: Type
foreign import data Address :: Type
foreign import data BaseAddress :: Type
foreign import data AuxiliaryData :: Type -- We don't use it for now
foreign import data Transaction :: Type
foreign import data TransactionBody :: Type
foreign import data TransactionWitnessSet :: Type
foreign import data TransactionHash :: Type
foreign import data TransactionInput :: Type
foreign import data TransactionInputs :: Type
foreign import data TransactionOutput :: Type
foreign import data TransactionOutputs :: Type
foreign import data StakeCredential :: Type
foreign import data Ed25519KeyHash :: Type
foreign import data ScriptHash :: Type
foreign import data MultiAsset :: Type
foreign import data Assets :: Type
foreign import data AssetName :: Type
foreign import data DataHash :: Type
foreign import data Vkeywitnesses :: Type
foreign import data Vkeywitness :: Type
foreign import data Vkey :: Type
foreign import data Ed25519Signature :: Type
foreign import data PublicKey :: Type
foreign import data PlutusScript :: Type
foreign import data PlutusScripts :: Type

foreign import newBigNum :: String -> Effect BigNum
foreign import newValue :: BigNum -> Effect Value
foreign import newValueFromAssets :: MultiAsset -> Effect Value
foreign import newTransactionHash :: Uint8Array -> Effect TransactionHash
foreign import newTransactionInput :: TransactionHash -> Number -> Effect TransactionInput
foreign import newTransactionInputs :: Effect TransactionInputs
foreign import addTransactionInput :: TransactionInputs -> TransactionInput -> Effect Unit
foreign import newTransactionOutput :: Address -> Value -> Effect TransactionOutput
foreign import newTransactionOutputs :: Effect TransactionOutputs
foreign import addTransactionOutput :: TransactionOutputs -> TransactionOutput -> Effect Unit
foreign import newTransactionBody :: TransactionInputs -> TransactionOutputs -> BigNum -> Effect TransactionBody
foreign import newTransactionBody_ :: TransactionInputs -> TransactionOutputs -> BigNum -> Number -> Effect TransactionBody
foreign import newTransaction :: TransactionBody -> TransactionWitnessSet -> Effect Transaction
foreign import newTransaction_ :: TransactionBody -> TransactionWitnessSet -> AuxiliaryData -> Effect Transaction
foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newBaseAddress :: Int -> StakeCredential -> StakeCredential -> Effect BaseAddress
foreign import baseAddressToAddress :: BaseAddress -> Effect Address
foreign import newStakeCredentialFromScriptHash :: ScriptHash -> Effect StakeCredential -- TODO: is not needed?
foreign import newStakeCredentialFromKeyHash :: Ed25519KeyHash -> Effect StakeCredential
foreign import newEd25519KeyHash :: Uint8Array -> Effect Ed25519KeyHash
foreign import newMultiAsset :: Effect MultiAsset
foreign import insertMultiAsset :: MultiAsset -> ScriptHash -> Assets -> Effect Unit
foreign import newAssets :: Effect Assets
foreign import insertAssets :: Assets -> AssetName -> BigNum -> Effect Unit
foreign import newAssetName :: Uint8Array -> Effect AssetName
foreign import newScriptHash :: Uint8Array -> Effect ScriptHash
foreign import newDataHash :: Uint8Array -> Effect DataHash
foreign import transactionOutputSetDataHash :: TransactionOutput -> DataHash -> Effect Unit
foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey
foreign import newPublicKey :: T.Bech32 -> Effect PublicKey
foreign import newEd25519Signature :: T.Bech32 -> Effect Ed25519Signature
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import transactionWitnessSetSetVkeys :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit
foreign import newPlutusScript :: Uint8Array -> Effect PlutusScript
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import txWitnessSetSetPlutusScripts :: TransactionWitnessSet -> PlutusScripts -> Effect Unit
foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit

convertTransaction :: T.Transaction -> Effect Transaction
convertTransaction (T.Transaction { body: T.TxBody body, witness_set }) = do
  inputs <- convertTxInputs body.inputs
  outputs <- convertTxOutputs body.outputs
  fee <- convertFee body.fee
  txBody <- newTransactionBody inputs outputs fee
  ws <- convertWitnessSet witness_set
  newTransaction txBody ws

convertFee :: T.Coin -> Effect BigNum
convertFee (T.Coin bigInt) = newBigNum (BigInt.toString bigInt)

convertWitnessSet :: T.TransactionWitnessSet -> Effect TransactionWitnessSet
convertWitnessSet (T.TransactionWitnessSet { vkeys, native_scripts, bootstraps, plutus_scripts, plutus_data, redeemers } ) = do
  ws <- newTransactionWitnessSet
  for_ vkeys
    (convertVkeywitnesses >=> transactionWitnessSetSetVkeys ws)
  for_ plutus_scripts \ps -> do
    scripts <- newPlutusScripts
    for_ ps (convertPlutusScript >=> addPlutusScript scripts)
    txWitnessSetSetPlutusScripts ws scripts
  -- TODO: native_scripts
  -- TODO: bootstraps
  -- TODO: plutus_data
  -- TODO: redeemers
  pure ws

convertPlutusScript :: T.PlutusScript -> Effect PlutusScript
convertPlutusScript (T.PlutusScript bytes) = do
  newPlutusScript bytes

convertVkeywitnesses :: Array T.Vkeywitness -> Effect Vkeywitnesses
convertVkeywitnesses arr = do
  witnesses <- newVkeywitnesses
  traverse_ (convertVkeywitness >=> addVkeywitness witnesses) arr
  pure witnesses

convertVkeywitness :: T.Vkeywitness -> Effect Vkeywitness
convertVkeywitness (T.Vkeywitness (vkey /\ signature)) = do
  vkey' <- convertVkey vkey
  signature' <- convertEd25519Signature signature
  newVkeywitness vkey' signature'

convertEd25519Signature :: T.Ed25519Signature -> Effect Ed25519Signature
convertEd25519Signature (T.Ed25519Signature bech32) =
  newEd25519Signature bech32

convertVkey :: T.Vkey -> Effect Vkey
convertVkey (T.Vkey (T.PublicKey pk)) =
  newPublicKey pk >>= newVkeyFromPublicKey

convertTxInputs :: Array T.TransactionInput -> Effect TransactionInputs
convertTxInputs arrInputs = do
  inputs <- newTransactionInputs
  traverse_ (convertTxInput >=> addTransactionInput inputs) arrInputs
  pure inputs

convertTxInput :: T.TransactionInput -> Effect TransactionInput
convertTxInput (T.TransactionInput { transaction_id, index }) = do
  tx_hash <- newTransactionHash (unwrap transaction_id)
  newTransactionInput tx_hash
    (BigInt.toNumber index) -- u32, so no precision loss here

convertTxOutputs :: Array T.TransactionOutput -> Effect TransactionOutputs
convertTxOutputs arrOutputs = do
  outputs <- newTransactionOutputs
  traverse_ (convertTxOutput >=> addTransactionOutput outputs) arrOutputs
  pure outputs

convertTxOutput :: T.TransactionOutput -> Effect TransactionOutput
convertTxOutput (T.TransactionOutput { address, amount, data_hash }) = do
  let
    baseAddress = unwrap (unwrap address)."AddrType"
  payment <- convertCredential baseAddress.payment
  stake <- convertCredential baseAddress.stake
  base_address <- newBaseAddress baseAddress.network payment stake
  address' <- baseAddressToAddress base_address
  value <- convertValue amount
  txo <- newTransactionOutput address' value
  for_ data_hash (unwrap >>> newDataHash >=> transactionOutputSetDataHash txo)
  pure txo

convertValue :: T.Value -> Effect Value
convertValue (T.Value m) = do
  multiasset <- newMultiAsset
  void $ forWithIndex m \(T.CurrencySymbol symbol) values -> do
    assets <- newAssets
    void $ forWithIndex values \(T.TokenName tokenName) bigIntValue -> do
      assetName <- newAssetName tokenName
      value <- newBigNum (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    scripthash <- newScriptHash symbol
    insertMultiAsset multiasset scripthash assets
  newValueFromAssets multiasset

convertCredential :: T.Credential -> Effect StakeCredential
convertCredential (T.Credential bytes) = do
  keyHash <- newEd25519KeyHash bytes
  newStakeCredentialFromKeyHash keyHash
