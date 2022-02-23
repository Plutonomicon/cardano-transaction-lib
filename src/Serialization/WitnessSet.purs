module Serialization.WitnessSet where

import Prelude

import Data.Array as Array
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytesEffect)
import Serialization.PlutusData as Serialization.PlutusData
import Effect (Effect)
import Effect.Exception (throw)
import FfiHelpers (ContainerHelper, containerHelper)
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.NativeScript (convertNativeScripts)
import Serialization.Types
  ( BigNum
  , BootstrapWitness
  , Ed25519Signature
  , ExUnits
  , NativeScripts
  , PlutusData
  , PlutusScript
  , PlutusScripts
  , PublicKey
  , Redeemer
  , RedeemerTag
  , TransactionWitnessSet
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  )
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray)
import Types.PlutusData as PlutusData
import Types.RedeemerTag as Tag
import Types.Transaction as T

setPlutusData :: PlutusData -> TransactionWitnessSet -> Effect TransactionWitnessSet
setPlutusData pd ws = _wsSetPlutusData containerHelper ws (Array.singleton pd)
  *> pure ws

convertWitnessSet :: T.TransactionWitnessSet -> Effect TransactionWitnessSet
convertWitnessSet (T.TransactionWitnessSet tws) = do
  ws <- newTransactionWitnessSet
  for_ tws.vkeys
    (convertVkeywitnesses >=> transactionWitnessSetSetVkeys ws)
  for_ tws.native_scripts $
    maybe (throw "Failed to convert NativeScripts")
      (transactionWitnessSetSetNativeScripts ws) <<< convertNativeScripts
  for_ tws.bootstraps
    (traverse convertBootstrap >=> _wsSetBootstraps containerHelper ws)
  for_ tws.plutus_scripts \ps -> do
    scripts <- newPlutusScripts
    for_ ps (convertPlutusScript >=> addPlutusScript scripts)
    txWitnessSetSetPlutusScripts ws scripts
  for_ tws.plutus_data
    (traverse convertPlutusData >=> _wsSetPlutusData containerHelper ws)
  for_ tws.redeemers
    (traverse convertRedeemer >=> _wsSetRedeemers containerHelper ws)
  pure ws

convertRedeemer :: T.Redeemer -> Effect Redeemer
convertRedeemer (T.Redeemer { tag, index, "data": data_, ex_units }) = do
  tag' <- convertRedeemerTag tag
  index' <- maybe (throw "Failed to convert redeemer index") pure $ bigNumFromBigInt index
  data' <- convertPlutusData data_
  ex_units' <- convertExUnits ex_units
  newRedeemer tag' index' data' ex_units'

convertRedeemerTag :: Tag.RedeemerTag -> Effect RedeemerTag
convertRedeemerTag = _newRedeemerTag <<< case _ of
  Tag.Spend -> "spend"
  Tag.Mint -> "mint"
  Tag.Cert -> "cert"
  Tag.Reward -> "reward"

convertExUnits :: T.ExUnits -> Effect ExUnits
convertExUnits { mem, steps } =
  maybe (throw "Failed to construct ExUnits") pure do
    mem' <- bigNumFromBigInt mem
    steps' <- bigNumFromBigInt steps
    pure $ newExUnits mem' steps'

convertPlutusData :: T.PlutusData -> Effect PlutusData
convertPlutusData = fromBytesEffect <<< unwrap

convertBootstrap :: T.BootstrapWitness -> Effect BootstrapWitness
convertBootstrap { vkey, signature, chain_code, attributes } = do
  vkey' <- convertVkey vkey
  signature' <- convertEd25519Signature signature
  newBootstrapWitness vkey' signature' chain_code attributes

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

foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newEd25519Signature :: Bech32String -> Effect Ed25519Signature
foreign import newPublicKey :: Bech32String -> Effect PublicKey
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey
foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import newPlutusScript :: ByteArray -> Effect PlutusScript
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit
foreign import transactionWitnessSetSetVkeys :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit
foreign import txWitnessSetSetPlutusScripts :: TransactionWitnessSet -> PlutusScripts -> Effect Unit
foreign import transactionWitnessSetSetNativeScripts :: TransactionWitnessSet -> NativeScripts -> Effect Unit
foreign import _wsSetBootstraps :: ContainerHelper -> TransactionWitnessSet -> Array BootstrapWitness -> Effect Unit
foreign import newBootstrapWitness :: Vkey -> Ed25519Signature -> ByteArray -> ByteArray -> Effect BootstrapWitness
foreign import _wsSetPlutusData :: ContainerHelper -> TransactionWitnessSet -> Array PlutusData -> Effect Unit
foreign import newRedeemer :: RedeemerTag -> BigNum -> PlutusData -> ExUnits -> Effect Redeemer
foreign import _newRedeemerTag :: String -> Effect RedeemerTag
foreign import newExUnits :: BigNum -> BigNum -> ExUnits
foreign import _wsSetRedeemers :: ContainerHelper -> TransactionWitnessSet -> Array Redeemer -> Effect Unit
