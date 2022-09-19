module Serialization.WitnessSet
  ( setPlutusData
  , setRedeemers
  , setPlutusScripts
  , convertWitnessSet
  , convertRedeemers
  , convertRedeemer
  , convertPlutusDataEffect
  , convertRedeemerTag
  , convertExUnits
  , convertBootstrap
  , convertVkeywitnesses
  , convertVkeywitness
  , convertEd25519Signature
  , convertVkey
  , newTransactionWitnessSet
  , newEd25519Signature
  , newPublicKey
  , newVkeyFromPublicKey
  , newVkeywitnesses
  , newVkeywitness
  , addVkeywitness
  , newPlutusScripts
  , addPlutusScript
  , transactionWitnessSetSetVkeys
  , txWitnessSetSetPlutusScripts
  , transactionWitnessSetSetNativeScripts
  , _wsSetBootstraps
  , newBootstrapWitness
  , _wsSetPlutusData
  , newRedeemer
  , _newRedeemerTag
  , newExUnits
  , _wsSetRedeemers
  , _mkRedeemers
  , _wsSetPlutusScripts
  ) where

import Prelude

import Cardano.Types.Transaction
  ( BootstrapWitness
  , Ed25519Signature(Ed25519Signature)
  , ExUnits
  , Redeemer(Redeemer)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  ) as T
import Cardano.Types.Transaction (convertPubKey)
import Data.Maybe (maybe)
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytes')
import Effect (Effect)
import Effect.Exception (throw)
import FfiHelpers (ContainerHelper, containerHelper)
import Serialization.NativeScript (convertNativeScripts)
import Serialization.PlutusData (convertPlutusData)
import Serialization.PlutusScript (convertPlutusScript)
import Serialization.Types
  ( BootstrapWitness
  , Ed25519Signature
  , ExUnits
  , NativeScripts
  , PlutusScript
  , PlutusScripts
  , PublicKey
  , Redeemer
  , Redeemers
  , RedeemerTag
  , TransactionWitnessSet
  , Vkey
  , Vkeywitness
  , Vkeywitnesses
  )
import Serialization.Types (PlutusData) as PDS
import Types.Aliases (Bech32String)
import Types.BigNum (BigNum)
import Types.BigNum (fromBigInt) as BigNum
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData) as PD
import Types.RedeemerTag as Tag

setPlutusData :: Array PDS.PlutusData -> TransactionWitnessSet -> Effect Unit
setPlutusData pd ws = setWitnesses _wsSetPlutusData ws pd

setRedeemers :: Array Redeemer -> TransactionWitnessSet -> Effect Unit
setRedeemers rs ws = setWitnesses _wsSetRedeemers ws rs

setPlutusScripts :: Array PlutusScript -> TransactionWitnessSet -> Effect Unit
setPlutusScripts ps ws = setWitnesses _wsSetPlutusScripts ws ps

setWitnesses
  :: forall (a :: Type)
   . (ContainerHelper -> TransactionWitnessSet -> Array a -> Effect Unit)
  -> TransactionWitnessSet
  -> Array a
  -> Effect Unit
setWitnesses f ws = f containerHelper ws

convertWitnessSet :: T.TransactionWitnessSet -> Effect TransactionWitnessSet
convertWitnessSet (T.TransactionWitnessSet tws) = do
  ws <- newTransactionWitnessSet
  for_ tws.vkeys
    (convertVkeywitnesses >=> transactionWitnessSetSetVkeys ws)
  for_ tws.nativeScripts $
    maybe (throw "Failed to convert NativeScripts")
      (transactionWitnessSetSetNativeScripts ws) <<< convertNativeScripts
  for_ tws.bootstraps
    (traverse convertBootstrap >=> _wsSetBootstraps containerHelper ws)
  for_ tws.plutusScripts \ps -> do
    scripts <- newPlutusScripts
    for_ ps (convertPlutusScript >>> addPlutusScript scripts)
    txWitnessSetSetPlutusScripts ws scripts
  for_ tws.plutusData
    (traverse convertPlutusDataEffect >=> _wsSetPlutusData containerHelper ws)
  for_ tws.redeemers
    (traverse convertRedeemer >=> _wsSetRedeemers containerHelper ws)
  pure ws

convertRedeemers :: Array T.Redeemer -> Effect Redeemers
convertRedeemers redeemers = do
  _mkRedeemers containerHelper <$> traverse convertRedeemer redeemers

convertRedeemer :: T.Redeemer -> Effect Redeemer
convertRedeemer (T.Redeemer { tag, index, "data": data_, exUnits }) = do
  tag' <- convertRedeemerTag tag
  index' <- maybe (throw "Failed to convert redeemer index") pure $
    BigNum.fromBigInt index
  data' <- convertPlutusDataEffect data_
  exUnits' <- convertExUnits exUnits
  newRedeemer tag' index' data' exUnits'

convertPlutusDataEffect :: PD.PlutusData -> Effect PDS.PlutusData
convertPlutusDataEffect pd = maybe (throw "Failed to convert PlutusData") pure $
  convertPlutusData
    pd

convertRedeemerTag :: Tag.RedeemerTag -> Effect RedeemerTag
convertRedeemerTag = _newRedeemerTag <<< case _ of
  Tag.Spend -> "spend"
  Tag.Mint -> "mint"
  Tag.Cert -> "cert"
  Tag.Reward -> "reward"

convertExUnits :: T.ExUnits -> Effect ExUnits
convertExUnits { mem, steps } =
  maybe (throw "Failed to construct ExUnits") pure do
    mem' <- BigNum.fromBigInt mem
    steps' <- BigNum.fromBigInt steps
    pure $ newExUnits mem' steps'

convertBootstrap :: T.BootstrapWitness -> Effect BootstrapWitness
convertBootstrap { vkey, signature, chainCode, attributes } = do
  vkey' <- convertVkey vkey
  signature' <- convertEd25519Signature signature
  newBootstrapWitness vkey' signature' chainCode attributes

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
convertVkey (T.Vkey pk) = newVkeyFromPublicKey $ convertPubKey pk

foreign import newTransactionWitnessSet :: Effect TransactionWitnessSet
foreign import newEd25519Signature :: Bech32String -> Effect Ed25519Signature
foreign import newPublicKey :: Bech32String -> Effect PublicKey
foreign import newVkeyFromPublicKey :: PublicKey -> Effect Vkey
foreign import newVkeywitnesses :: Effect Vkeywitnesses
foreign import newVkeywitness :: Vkey -> Ed25519Signature -> Effect Vkeywitness
foreign import addVkeywitness :: Vkeywitnesses -> Vkeywitness -> Effect Unit
foreign import newPlutusScripts :: Effect PlutusScripts
foreign import addPlutusScript :: PlutusScripts -> PlutusScript -> Effect Unit
foreign import transactionWitnessSetSetVkeys
  :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit

foreign import txWitnessSetSetPlutusScripts
  :: TransactionWitnessSet -> PlutusScripts -> Effect Unit

foreign import transactionWitnessSetSetNativeScripts
  :: TransactionWitnessSet -> NativeScripts -> Effect Unit

foreign import _wsSetBootstraps
  :: ContainerHelper
  -> TransactionWitnessSet
  -> Array BootstrapWitness
  -> Effect Unit

foreign import newBootstrapWitness
  :: Vkey
  -> Ed25519Signature
  -> ByteArray
  -> ByteArray
  -> Effect BootstrapWitness

foreign import _wsSetPlutusData
  :: ContainerHelper
  -> TransactionWitnessSet
  -> Array PDS.PlutusData
  -> Effect Unit

foreign import newRedeemer
  :: RedeemerTag -> BigNum -> PDS.PlutusData -> ExUnits -> Effect Redeemer

foreign import _newRedeemerTag :: String -> Effect RedeemerTag
foreign import newExUnits :: BigNum -> BigNum -> ExUnits
foreign import _wsSetRedeemers
  :: ContainerHelper -> TransactionWitnessSet -> Array Redeemer -> Effect Unit

foreign import _mkRedeemers :: ContainerHelper -> Array Redeemer -> Redeemers
foreign import _wsSetPlutusScripts
  :: ContainerHelper
  -> TransactionWitnessSet
  -> Array PlutusScript
  -> Effect Unit
