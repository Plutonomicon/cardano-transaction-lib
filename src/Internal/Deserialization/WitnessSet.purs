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
  , ExUnits
  , Redeemer(Redeemer)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , mkFromCslEd25519Signature
  , mkFromCslPubKey
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
import Data.Maybe (Maybe)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

convertWitnessSet :: TransactionWitnessSet -> T.TransactionWitnessSet
convertWitnessSet ws =
  let
    vkeys = getVkeywitnesses maybeFfiHelper ws <#> convertVkeyWitnesses
    nativeScripts = getNativeScripts maybeFfiHelper ws <#> convertNativeScripts
    bootstraps = getBootstraps maybeFfiHelper ws <#> convertBootstraps
    plutusScripts = getPlutusScripts maybeFfiHelper ws <#> convertPlutusScripts
    plutusData = getWitnessSetPlutusData maybeFfiHelper ws <#> convertPlutusList
    redeemers = getRedeemers maybeFfiHelper ws <#> convertRedeemers
  in
    T.TransactionWitnessSet
      { vkeys
      , nativeScripts
      , bootstraps
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
    signature = T.mkFromCslEd25519Signature $ getSignature witness
  in
    T.Vkeywitness $ publicKey /\ signature

convertVkey :: Vkey -> T.Vkey
convertVkey = T.Vkey <<< T.mkFromCslPubKey <<< vkeyPublicKey

convertNativeScripts :: NativeScripts -> Array T.NativeScript
convertNativeScripts nativeScripts =
  extractNativeScripts nativeScripts <#> convertNativeScript

convertBootstraps :: BootstrapWitnesses -> Array T.BootstrapWitness
convertBootstraps = extractBootstraps >>> map \bootstrap ->
  { vkey: convertVkey $ getBootstrapVkey bootstrap
  , signature: T.mkFromCslEd25519Signature $ getBootstrapSignature bootstrap
  , chainCode: getBootstrapChainCode bootstrap
  , attributes: getBootstrapAttributes bootstrap
  }

convertPlutusScripts :: PlutusScripts -> Array S.PlutusScript
convertPlutusScripts plutusScripts =
  extractPlutusScripts plutusScripts <#> convertPlutusScript

convertPlutusScript :: PlutusScript -> S.PlutusScript
convertPlutusScript plutusScript = do
  let
    language = convertLanguage $ plutusScriptVersion plutusScript
  curry S.PlutusScript (plutusScriptBytes plutusScript) language

convertPlutusList :: PlutusList -> Array T.PlutusData
convertPlutusList = extractPlutusData >>> map convertPlutusData

convertRedeemers :: Redeemers -> Array T.Redeemer
convertRedeemers = extractRedeemers >>> map convertRedeemer

convertRedeemer :: Redeemer -> T.Redeemer
convertRedeemer redeemer =
  let
    tag = convertRedeemerTag $ getRedeemerTag redeemer
    index = BigNum.toBigInt $ getRedeemerIndex redeemer
    exUnits = convertExUnits $ getExUnits redeemer
    data_ = convertPlutusData $ getRedeemerPlutusData redeemer
  in
    T.Redeemer
      { tag
      , index
      , data: data_
      , exUnits
      }

convertRedeemerTag :: RedeemerTag -> Tag.RedeemerTag
convertRedeemerTag tag = case getRedeemerTagKind tag of
  0 -> Tag.Spend
  1 -> Tag.Mint
  2 -> Tag.Cert
  3 -> Tag.Reward
  _ -> unsafePerformEffect $ throw "convertRedeemerTag: impossible happened"

convertExUnits :: ExUnits -> T.ExUnits
convertExUnits eu =
  let
    mem = BigNum.toBigInt $ getExUnitsMem eu
    steps = BigNum.toBigInt $ getExUnitsSteps eu
  in
    { mem, steps }

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
