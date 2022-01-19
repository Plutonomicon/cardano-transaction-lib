module Serialize where

import Data.BigInt as BigInt
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse_, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Data.Newtype (unwrap)
import Prelude
import Types.Transaction as T


-- * cardano-serialization-lib types

foreign import data BigNum :: Type
foreign import data Value :: Type
foreign import data Address :: Type
foreign import data BaseAddress :: Type
foreign import data Amount :: Type
foreign import data AuxiliaryData :: Type -- We don't use it for now
foreign import data Transaction :: Type
foreign import data TransactionBody :: Type
foreign import data TransactionWitnessSet :: Type
foreign import data TransactionHash :: Type
foreign import data TransactionInput :: Type
foreign import data TransactionInputs :: Type
foreign import data TransactionOutput :: Type
foreign import data TransactionOutputs :: Type
foreign import data TransactionFee :: Type
foreign import data StakeCredential :: Type
foreign import data Ed25519KeyHash :: Type
foreign import data ScriptHash :: Type
foreign import data MultiAsset :: Type
foreign import data Assets :: Type
foreign import data AssetName :: Type


foreign import newBigNum :: String -> Effect BigNum
foreign import newValue :: BigNum -> Effect Value
foreign import newValueFromAssets :: MultiAsset -> Effect Value
foreign import newAddress :: String -> Effect Address
foreign import newTransactionHash :: String -> Effect TransactionHash
foreign import newTransactionInput :: TransactionHash -> Number -> Effect TransactionInput
foreign import newTransactionInputs :: Effect TransactionInputs
foreign import addTransactionInput :: TransactionInputs -> TransactionInput -> Effect Unit
foreign import newTransactionOutput :: Address -> Amount -> Effect TransactionOutput
foreign import newTransactionOutputs :: Effect TransactionOutputs
foreign import addTransactionOutput :: TransactionOutputs -> TransactionOutput -> Effect Unit
foreign import newTransactionBody :: TransactionInputs -> TransactionOutputs -> TransactionFee -> Effect TransactionBody
foreign import newTransaction :: TransactionBody -> TransactionWitnessSet -> Effect Transaction
foreign import newTransaction_ :: TransactionBody -> TransactionWitnessSet -> AuxiliaryData -> Effect Transaction
foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newBaseAddress :: Int -> StakeCredential -> StakeCredential -> Effect BaseAddress
foreign import baseAddressToAddress :: BaseAddress -> Effect Address
foreign import newStakeCredentialFromScriptHash :: ScriptHash -> Effect StakeCredential
foreign import newStakeCredentialFromKeyHash :: Ed25519KeyHash -> Effect StakeCredential
foreign import newEd25519KeyHash :: String -> Effect Ed25519KeyHash
foreign import newMultiAsset :: Effect MultiAsset
foreign import insertMultiAsset :: MultiAsset -> ScriptHash -> Assets -> Effect Unit
foreign import newAssets :: Effect Assets
foreign import insertAssets :: Assets -> AssetName -> BigNum -> Effect Unit

-- convertTransaction :: T.Transaction -> Effect Transaction
-- convertTransaction (T.Transaction { body, witness_set }) = do
--   inputs <- convertTxInputs body.inputs
--   outputs <- convertTxOutputs body.outputs
--   pure unit

convertTxInputs :: Array T.TransactionInput -> Effect TransactionInputs
convertTxInputs arrInputs = do
  inputs <- newTransactionInputs
  traverse_ (convertTxInput >=> addTransactionInput inputs) arrInputs
  pure inputs

convertTxInput :: T.TransactionInput -> Effect TransactionInput
convertTxInput (T.TransactionInput { transaction_id, index }) = do
  tx_hash <- newTransactionHash transaction_id
  newTransactionInput tx_hash
    (BigInt.toNumber index) -- u32, so no precision loss here

-- convertTxOutputs :: Array T.TransactionOutput -> Effect TransactionInputs
-- convertTxOutputs arrOutputs = do
--   outputs <- newTransactionOutputs
--   traverse_ (convertTxOutput >=> addTransactionOutput outputs) arrOutputs
--   pure outputs

-- convertTxOutput :: T.TransactionOutput -> Effect TransactionOutput
-- convertTxOutput (T.TransactionOutput { address, amount, data_hash }) = do
--   let
--     baseAddress = (unwrap address)."AddrType"
--   payment <- convertCredential baseAddress.payment
--   stake <- convertCredential baseAddress.stake
--   baseAddress <- newBaseAddress baseAddress.network payment stake
--   address <- baseAddressToAddress baseAddress
--   newTransactionOutput address amount

-- convertValue :: T.Value -> Effect Value
-- convertValue (T.Value m) = do
--   multiasset <- newMultiAsset
--   void $ forWithIndex m \currencySymbol values -> do
--     assets <- newAssets
--     void $ forWithIndex values \tokenName bigInt -> do
--       pure unit
--     insertMultiAsset multiasset scripthash assets
--   newValueFromAssets multiasset

convertCredential :: T.Credential -> Effect StakeCredential
convertCredential (T.Credential str) = do
  -- TODO: am I correct? keyhash or scripthash?
  keyHash <- newEd25519KeyHash str
  newStakeCredentialFromKeyHash keyHash
