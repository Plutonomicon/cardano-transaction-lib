module Serialization
  ( convertTransaction
  , addressBech32
  , addressPubKeyHash
  , convertAddress
  , convertBigInt
  , convertTxInput
  , convertTxOutput
  , newAddressFromBech32
  , newAddressFromBytes
  , newAssetName
  , toBytes
  , newBaseAddressFromAddress
  , newTransactionUnspentOutputFromBytes
  , newTransactionWitnessSetFromBytes
  ) where

import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.UInt (UInt)
import Effect (Effect)
import Prelude
import Serialization.Types
  ( Address
  , AssetName
  , Assets
  , AuxiliaryData
  , BaseAddress
  , BigNum
  , DataHash
  , Ed25519KeyHash
  , Ed25519Signature
  , MultiAsset
  , PlutusData
  , PlutusScript
  , PlutusScripts
  , PublicKey
  , ScriptHash
  , StakeCredential
  , Transaction
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionOutput
  , TransactionOutputs
  , TransactionUnspentOutput
  , TransactionWitnessSet
  , Value
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  )
import Types.ByteArray (ByteArray)
import Types.Transaction as T
import Types.Value as Value
import Untagged.Union (type (|+|))
import Deserialization.FromBytes (fromBytes, fromBytesEffect)

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
foreign import newAddressFromBech32 :: T.Bech32 -> Effect Address
foreign import newAddressFromBytes :: ByteArray -> Effect Address
foreign import newBaseAddress :: UInt -> StakeCredential -> StakeCredential -> Effect BaseAddress
foreign import _newBaseAddressFromAddress :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Address -> Maybe BaseAddress
foreign import baseAddressPaymentCredential :: BaseAddress -> StakeCredential
foreign import baseAddressToAddress :: BaseAddress -> Effect Address
foreign import newStakeCredentialFromScriptHash :: ScriptHash -> Effect StakeCredential
foreign import newStakeCredentialFromKeyHash :: Ed25519KeyHash -> Effect StakeCredential
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
foreign import newPublicKey :: T.Bech32 -> Effect PublicKey
foreign import newEd25519Signature :: T.Bech32 -> Effect Ed25519Signature
foreign import transactionWitnessSetSetVkeys :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit
foreign import newPlutusScript :: ByteArray -> Effect PlutusScript
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import txWitnessSetSetPlutusScripts :: TransactionWitnessSet -> PlutusScripts -> Effect Unit
foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit
foreign import _addressPubKeyHash :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> BaseAddress -> Maybe T.Bech32
foreign import _addressBech32 :: Address -> T.Bech32

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

addressPubKeyHash :: BaseAddress -> Maybe T.Bech32
addressPubKeyHash = _addressPubKeyHash Just Nothing

addressBech32 :: Address -> T.Bech32
addressBech32 = _addressBech32

newBaseAddressFromAddress :: Address -> Maybe BaseAddress
newBaseAddressFromAddress = _newBaseAddressFromAddress Nothing Just

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
  address' <- convertAddress address
  value <- convertValue amount
  txo <- newTransactionOutput address' value
  for_ (unwrap <$> data_hash) \bytes -> do
    for_ (fromBytes bytes) $
      transactionOutputSetDataHash txo
  pure txo

convertAddress :: T.Address -> Effect Address
convertAddress address = do
  let baseAddress = unwrap (unwrap address)."AddrType"
  payment <- case baseAddress.payment of
    T.PaymentCredentialKey (T.Ed25519KeyHash keyHashBytes) -> do
      newStakeCredentialFromKeyHash =<< fromBytesEffect keyHashBytes
    T.PaymentCredentialScript (T.ScriptHash scriptHashBytes) -> do
      newStakeCredentialFromScriptHash =<< fromBytesEffect scriptHashBytes
  stake <- case baseAddress.stake of
    T.StakeCredentialKey (T.Ed25519KeyHash keyHashBytes) -> do
      newStakeCredentialFromKeyHash =<< fromBytesEffect keyHashBytes
    T.StakeCredentialScript (T.ScriptHash scriptHashBytes) -> do
      newStakeCredentialFromScriptHash =<< fromBytesEffect scriptHashBytes
  base_address <- newBaseAddress baseAddress.network payment stake
  baseAddressToAddress base_address

convertValue :: Value.Value -> Effect Value
convertValue val = do
  let
    lovelace = Value.valueToCoin' val
    m = Value.getNonAdaAsset' val
  multiasset <- newMultiAsset
  forWithIndex_ m \scriptHashBytes' values -> do
    let scriptHashBytes = Value.getCurrencySymbol scriptHashBytes'
    assets <- newAssets
    forWithIndex_ values \tokenName' bigIntValue -> do
      let tokenName = Value.getTokenName tokenName'
      assetName <- newAssetName tokenName
      value <- newBigNum (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    scripthash <- fromBytesEffect scriptHashBytes
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< newBigNum (BigInt.toString lovelace)
  pure value
