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
import Contract.Monad (Contract, liftContractAffM, liftContractM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Examples.Helpers.LoadScript (loadScript)
import Data.Map as Map
import Data.Set as Set

newtype SchnorrRedeemer = SchnorrRedeemer
  { msg :: ByteArray
  , sig :: SchnorrSignature
  , pk :: SchnorrPublicKey
  }

derive instance Generic SchnorrRedeemer _
derive instance Newtype SchnorrRedeemer _

instance ToData SchnorrRedeemer where
  toData (SchnorrRedeemer { msg, sig, pk }) = Constr BigNum.zero
    [ toData msg, toData sig, toData pk ]

contract :: Contract Unit
contract = do
  void $ prepTest >>= testSchnorr

-- | Prepare the ECDSA test by locking some funds at the validator address
prepTest :: Contract TransactionHash
prepTest = do
  validator <- liftContractAffM "Caonnot get validator" getValidator
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf one

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator validator

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript valHash unitDatum
      Constraints.DatumInline
      val
  txId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted Schnorr test preparation tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction confirmed: " <> show txId

  pure txId

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification
  :: TransactionHash -> SchnorrRedeemer -> Contract TransactionHash
testVerification txId ecdsaRed = do
  let red = Redeemer $ toData ecdsaRed

  validator <- liftContractAffM "Can't get validator" getValidator
  let valHash = validatorHash validator

  netId <- getNetworkId
  valAddr <- liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos <- utxosAt valAddr
  txIn <- liftContractM "No UTxOs found at validator address"
    $ Set.toUnfoldable
    $ Set.filter (unwrap >>> _.transactionId >>> eq txId)
    $ Map.keys scriptUtxos

  let
    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs
        (Map.filterKeys ((unwrap >>> _.transactionId >>> eq txId)) scriptUtxos)

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput txIn red
  txId' <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted Schnorr test verification tx: " <> show txId'
  awaitTxConfirmed txId'
  logInfo' $ "Transaction confirmed: " <> show txId'
  pure txId'

-- | Testing ECDSA verification function on-chain
testSchnorr :: TransactionHash -> Contract TransactionHash
testSchnorr txId = do
  privateKey <- liftEffect $ randomSecp256k1PrivateKey
  let
    publicKey = deriveSchnorrSecp256k1PublicKey privateKey
    message = byteArrayFromIntArrayUnsafe [ 0, 1, 2, 3 ]
  signature <- liftAff $ signSchnorrSecp256k1 privateKey message
  testVerification txId $
    SchnorrRedeemer
      { msg: message
      , sig: signature
      , pk: publicKey
      }

getValidator :: Aff (Maybe Validator)
getValidator =
  loadScript "validate-schnorr.plutus" <#> \validateSchnorr -> do
    envelope <- decodeTextEnvelope validateSchnorr
    Validator <$> plutusScriptV2FromEnvelope envelope
