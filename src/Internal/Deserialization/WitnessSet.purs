module Ctl.Internal.Deserialization.WitnessSet
  ( convertNativeScripts
  , convertPlutusScripts
  , convertPlutusScript
  , convertVkeyWitnesses
  , convertVkeyWitness
  , convertWitnessSet
  , plutusScriptBytes
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.NativeScript (NativeScript) as T
import Ctl.Internal.Cardano.Types.Transaction
  ( BootstrapWitness
  , Ed25519Signature(Ed25519Signature)
  , ExUnits
  , PublicKey(PublicKey)
  , Redeemer(Redeemer)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as T
import Ctl.Internal.Deserialization.Language (convertLanguage)
import Ctl.Internal.Deserialization.NativeScript (convertNativeScript)
import Ctl.Internal.Deserialization.PlutusData (convertPlutusData)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types
  ( BootstrapWitness
  , BootstrapWitnesses
  , Ed25519Signature
  , ExUnits
  , Language
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
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (toBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.PlutusData (PlutusData) as T
import Ctl.Internal.Types.RedeemerTag as Tag
import Ctl.Internal.Types.Scripts (PlutusScript(PlutusScript)) as S
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for, traverse)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))

convertWitnessSet :: TransactionWitnessSet -> Maybe T.TransactionWitnessSet
convertWitnessSet ws = do
  nativeScripts <- for (getNativeScripts maybeFfiHelper ws) convertNativeScripts
  redeemers <- for (getRedeemers maybeFfiHelper ws) convertRedeemers
  plutusData <- for (getWitnessSetPlutusData maybeFfiHelper ws)
    convertPlutusList
  plutusScripts <- for (getPlutusScripts maybeFfiHelper ws) convertPlutusScripts
  pure $ T.TransactionWitnessSet
    { vkeys: getVkeywitnesses maybeFfiHelper ws <#> convertVkeyWitnesses
    , nativeScripts
    , bootstraps: getBootstraps maybeFfiHelper ws <#> convertBootstraps
    , plutusScripts
    , plutusData
    , redeemers
    }

convertVkeyWitnesses :: Vkeywitnesses -> Array T.Vkeywitness
convertVkeyWitnesses = extractWitnesses >>> map convertVkeyWitness

convertVkeyWitness :: Vkeywitness -> T.Vkeywitness
convertVkeyWitness witness =
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

convertPlutusScripts :: PlutusScripts -> Maybe (Array S.PlutusScript)
convertPlutusScripts plutusScripts =
  for (extractPlutusScripts plutusScripts) convertPlutusScript

convertPlutusScript :: PlutusScript -> Maybe S.PlutusScript
convertPlutusScript plutusScript =
  hush do
    language <- convertLanguage $ plutusScriptVersion plutusScript
    pure $ curry S.PlutusScript (plutusScriptBytes plutusScript) language

convertPlutusList :: PlutusList -> Maybe (Array T.PlutusData)
convertPlutusList = extractPlutusData >>> traverse convertPlutusData

convertRedeemers :: Redeemers -> Maybe (Array T.Redeemer)
convertRedeemers = extractRedeemers >>> traverse convertRedeemer

convertRedeemer :: Redeemer -> Maybe T.Redeemer
convertRedeemer redeemer = do
  tag <- convertRedeemerTag $ getRedeemerTag redeemer
  index <- BigNum.toBigInt $ getRedeemerIndex redeemer
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
  mem <- BigNum.toBigInt $ getExUnitsMem eu
  steps <- BigNum.toBigInt $ getExUnitsSteps eu
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
foreign import plutusScriptVersion :: PlutusScript -> Language
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
