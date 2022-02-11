module Serialization
  ( convertTransaction
  , addressBech32
  , addressPubKeyHash
  , convertAddress
  , convertTxInput
  , convertTxOutput
  , newAddressFromBech32
  , newAssetName
  , toBytes
  , newBaseAddressFromAddress
  ) where

import Data.BigInt as BigInt
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, for_)
import Data.UInt (UInt)
import Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Effect (Effect)
import Effect.Exception (throw)
import Prelude
import Serialization.BigNum (convertBigNum)
import Serialization.Types (Address, AssetName, Assets, AuxiliaryData, BaseAddress, BigNum, DataHash, Ed25519KeyHash, MultiAsset, PlutusData, ScriptHash, StakeCredential, Transaction, TransactionBody, TransactionHash, TransactionInput, TransactionInputs, TransactionOutput, TransactionOutputs, TransactionWitnessSet, Value)
import Serialization.WitnessSet (convertWitnessSet)
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
foreign import newAddressFromBech32 :: T.Bech32 -> Effect Address
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
         |+| TransactionWitnessSet
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
  fee <- maybe (throw "Failed to convert fee") pure $ convertBigNum (unwrap body.fee)
  txBody <- newTransactionBody inputs outputs fee
  ws <- convertWitnessSet witness_set
  newTransaction txBody ws

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
convertValue (Value.Value (Value.Coin lovelace) (Value.NonAdaAsset m)) = do
  multiasset <- newMultiAsset
  forWithIndex_ m \(Value.CurrencySymbol scriptHashBytes) values -> do
    assets <- newAssets
    forWithIndex_ values \(Value.TokenName tokenName) bigIntValue -> do
      assetName <- newAssetName tokenName
      value <- newBigNum (BigInt.toString bigIntValue)
      insertAssets assets assetName value
    scripthash <- fromBytesEffect scriptHashBytes
    insertMultiAsset multiasset scripthash assets
  value <- newValueFromAssets multiasset
  valueSetCoin value =<< newBigNum (BigInt.toString lovelace)
  pure value
