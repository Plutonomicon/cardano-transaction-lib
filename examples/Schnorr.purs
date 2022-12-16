module Ctl.Examples.Schnorr (contract) where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Crypto.Secp256k1.Schnorr
  ( SchnorrPublicKey
  , SchnorrSignature
  , deriveSchnorrSecp256k1PublicKey
  , signSchnorrSecp256k1
  )
import Contract.Crypto.Secp256k1.Utils (randomSecp256k1PrivateKey)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (filter, head)
import Data.Map as Map
import Effect.Aff (error)

newtype SchnorrRedeemer = SchnorrRedeemer
  { msg :: ByteArray
  , sig :: SchnorrSignature
  , pk :: SchnorrPublicKey
  }

derive instance Generic SchnorrRedeemer _
derive instance Newtype SchnorrRedeemer _

instance ToData SchnorrRedeemer where
  toData (SchnorrRedeemer { msg, sig, pk }) = Constr zero
    [ toData msg, toData sig, toData pk ]

contract :: Contract () Unit
contract = do
  txHash <- prepTest
  void $ testSchnorr txHash

-- | Prepare the ECDSA test by locking some funds at the validator address
prepTest :: Contract () TransactionHash
prepTest = do
  validator <- liftContractM "Caonnot get validator" getValidator
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf one

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator validator

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript valHash unitDatum
      Constraints.DatumInline
      val
  txHash <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted Schnorr test preparation tx: " <> show txHash
  awaitTxConfirmed txHash
  logInfo' $ "Transaction confirmed: " <> show txHash

  pure txHash

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification
  :: TransactionHash -> SchnorrRedeemer -> Contract () TransactionHash
testVerification txHash ecdsaRed = do
  let red = Redeemer $ toData ecdsaRed

  validator <- liftContractM "Can't get validator" getValidator
  let valHash = validatorHash validator

  netId <- getNetworkId
  valAddr <- liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos <- utxosAt valAddr
  (txIn /\ _) <- liftMaybe (error $ "cannot find transaction: " <> show txHash)
    $ head
    $ filter (\(input /\ _) -> (unwrap input).transactionId == txHash)
    $ Map.toUnfoldable scriptUtxos
  let
    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs scriptUtxos

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput txIn red
  -- logInfo' "waiting 20 secs"
  -- liftAff $ delay $ wrap 20000.0
  txId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted Schnorr test verification tx: " <> show txHash
  awaitTxConfirmed txId
  logInfo' $ "Transaction confirmed: " <> show txId
  pure txId

-- | Testing ECDSA verification function on-chain
testSchnorr :: TransactionHash -> Contract () TransactionHash
testSchnorr txHash = do
  privateKey <- liftEffect $ randomSecp256k1PrivateKey
  let
    publicKey = deriveSchnorrSecp256k1PublicKey privateKey
    message = byteArrayFromIntArrayUnsafe [ 0, 1, 2, 3 ]
  signature <- liftAff $ signSchnorrSecp256k1 privateKey message
  testVerification txHash $
    SchnorrRedeemer
      { msg: message
      , sig: signature
      , pk: publicKey
      }

getValidator :: Maybe Validator
getValidator = do
  decodeTextEnvelope validateSchnorr >>= plutusScriptV2FromEnvelope >>> map wrap

foreign import validateSchnorr :: String
