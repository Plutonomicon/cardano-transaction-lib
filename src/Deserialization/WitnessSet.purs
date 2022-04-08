module Deserialization.WitnessSet
  ( convertPlutusScripts
  , convertVkeyWitnesses
  , convertWitnessSet
  , deserializeWitnessSet
  , plutusScriptBytes
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Deserialization.BigNum (bigNumToBigInt)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Types
  ( BigNum
  , BootstrapWitness
  , BootstrapWitnesses
  , Ed25519Signature
  , ExUnits
  , NativeScript
  , NativeScripts
  , PlutusData
  , PlutusList
  , PlutusScript
  , PlutusScripts
  , PublicKey
  , Redeemer
  , RedeemerTag
  , Redeemers
  , TransactionWitnessSet
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  )
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData) as T
import Types.RedeemerTag as Tag
import Types.Transaction
  ( BootstrapWitness
  , Ed25519Signature(Ed25519Signature)
  , ExUnits
  , NativeScript
  , PublicKey(PublicKey)
  , Redeemer(Redeemer)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as T
import Types.Scripts (PlutusScript(PlutusScript)) as S
import Deserialization.NativeScript (convertNativeScript)
import Deserialization.PlutusData (convertPlutusData)

deserializeWitnessSet :: ByteArray -> Maybe TransactionWitnessSet
deserializeWitnessSet = _deserializeWitnessSet maybeFfiHelper

convertWitnessSet :: TransactionWitnessSet -> Maybe T.TransactionWitnessSet
convertWitnessSet ws = do
  nativeScripts <- for (getNativeScripts maybeFfiHelper ws) convertNativeScripts
  redeemers <- for (getRedeemers maybeFfiHelper ws) convertRedeemers
  plutusData <- for (getWitnessSetPlutusData maybeFfiHelper ws)
    convertPlutusList
  pure $ T.TransactionWitnessSet
    { vkeys: getVkeywitnesses maybeFfiHelper ws <#> convertVkeyWitnesses
    , nativeScripts
    , bootstraps: getBootstraps maybeFfiHelper ws <#> convertBootstraps
    , plutusScripts: getPlutusScripts maybeFfiHelper ws <#> convertPlutusScripts
    , plutusData
    , redeemers
    }

convertVkeyWitnesses :: Vkeywitnesses -> Array T.Vkeywitness
convertVkeyWitnesses = extractWitnesses >>> map \witness ->
  let
    vkey = getVkey witness
    publicKey = convertVkey vkey
    signature = convertSignature $ getSignature witness
  in
    T.Vkeywitness $ publicKey /\ signature

convertVkey :: Vkey -> T.Vkey
convertVkey = T.Vkey <<< T.PublicKey <<< publicKeyToBech32 <<< vkeyPublicKey

convertSignature :: Ed25519Signature -> T.Ed25519Signature
convertSignature = T.Ed25519Signature <<< signatureToBech32

convertNativeScripts :: NativeScripts -> Maybe (Array T.NativeScript)
convertNativeScripts nativeScripts =
  for (extractNativeScripts nativeScripts) convertNativeScript

convertBootstraps :: BootstrapWitnesses -> Array T.BootstrapWitness
convertBootstraps = extractBootstraps >>> map \bootstrap ->
  { vkey: convertVkey $ getBootstrapVkey bootstrap
  , signature: convertSignature $ getBootstrapSignature bootstrap
  , chainCode: getBootstrapChainCode bootstrap
  , attributes: getBootstrapAttributes bootstrap
  }

convertPlutusScripts :: PlutusScripts -> Array S.PlutusScript
convertPlutusScripts = extractPlutusScripts >>> map
  (plutusScriptBytes >>> S.PlutusScript)

convertPlutusList :: PlutusList -> Maybe (Array T.PlutusData)
convertPlutusList = extractPlutusData >>> traverse convertPlutusData

convertRedeemers :: Redeemers -> Maybe (Array T.Redeemer)
convertRedeemers = extractRedeemers >>> traverse convertRedeemer

convertRedeemer :: Redeemer -> Maybe T.Redeemer
convertRedeemer redeemer = do
  tag <- convertRedeemerTag $ getRedeemerTag redeemer
  index <- bigNumToBigInt $ getRedeemerIndex redeemer
  exUnits <- convertExUnits $ getExUnits redeemer
  data_ <- convertPlutusData $ getRedeemerPlutusData redeemer
  pure $ T.Redeemer
    { tag
    , index
    , data: data_
    , exUnits
    }

convertRedeemerTag :: RedeemerTag -> Maybe Tag.RedeemerTag
convertRedeemerTag tag = case getRedeemerTagKind tag of
  0 -> Just Tag.Spend
  1 -> Just Tag.Mint
  2 -> Just Tag.Cert
  3 -> Just Tag.Reward
  _ -> Nothing

convertExUnits :: ExUnits -> Maybe T.ExUnits
convertExUnits eu = do
  mem <- bigNumToBigInt $ getExUnitsMem eu
  steps <- bigNumToBigInt $ getExUnitsSteps eu
  pure { mem, steps }

foreign import getVkeywitnesses
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe Vkeywitnesses

foreign import extractWitnesses :: Vkeywitnesses -> Array Vkeywitness
foreign import getVkey :: Vkeywitness -> Vkey
foreign import getSignature :: Vkeywitness -> Ed25519Signature
foreign import vkeyPublicKey :: Vkey -> PublicKey
foreign import publicKeyToBech32 :: PublicKey -> String
foreign import signatureToBech32 :: Ed25519Signature -> String
foreign import getNativeScripts
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe NativeScripts

foreign import extractNativeScripts :: NativeScripts -> Array NativeScript
foreign import nativeScriptAs
  :: MaybeFfiHelper
  -> String
  -> T.NativeScript
  -> NativeScript
  -> Maybe T.NativeScript

foreign import getBootstraps
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe BootstrapWitnesses

foreign import extractBootstraps :: BootstrapWitnesses -> Array BootstrapWitness
foreign import getBootstrapVkey :: BootstrapWitness -> Vkey
foreign import getBootstrapSignature :: BootstrapWitness -> Ed25519Signature
foreign import getBootstrapChainCode :: BootstrapWitness -> ByteArray
foreign import getBootstrapAttributes :: BootstrapWitness -> ByteArray
foreign import getPlutusScripts
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe PlutusScripts

foreign import extractPlutusScripts :: PlutusScripts -> Array PlutusScript
foreign import plutusScriptBytes :: PlutusScript -> ByteArray
foreign import getWitnessSetPlutusData
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe PlutusList

foreign import extractPlutusData :: PlutusList -> Array PlutusData
foreign import getRedeemers
  :: MaybeFfiHelper -> TransactionWitnessSet -> Maybe Redeemers

foreign import extractRedeemers :: Redeemers -> Array Redeemer
foreign import getRedeemerTag :: Redeemer -> RedeemerTag
foreign import getRedeemerTagKind :: RedeemerTag -> Int
foreign import getRedeemerIndex :: Redeemer -> BigNum
foreign import getRedeemerPlutusData :: Redeemer -> PlutusData
foreign import getExUnits :: Redeemer -> ExUnits
foreign import getExUnitsMem :: ExUnits -> BigNum
foreign import getExUnitsSteps :: ExUnits -> BigNum
foreign import _deserializeWitnessSet
  :: MaybeFfiHelper -> ByteArray -> Maybe TransactionWitnessSet
