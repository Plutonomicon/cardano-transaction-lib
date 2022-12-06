module Ctl.Examples.ECDSA where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Examples.ECDSA.Script (ecdsaScript)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import Noble.Secp256k1.ECDSA (getECDSAPublicKey, signECDSA)
import Noble.Secp256k1.Utils (randomPrivateKey, sha256)
import Unsafe.Coerce (unsafeCoerce)

newtype ECDSARed = ECDSARed
  { msg ∷ ByteArray
  , sig ∷ ByteArray
  , pk ∷ ByteArray
  }

derive instance Generic ECDSARed _
derive instance Newtype ECDSARed _

instance ToData ECDSARed where
  toData (ECDSARed { msg, sig, pk }) = Constr zero
    [ toData msg, toData sig, toData pk ]

contract :: Contract () Unit
contract = do
  prepTest >>= traverse_ awaitTxConfirmed
  testECDSA >>= awaitTxConfirmed

-- | Prepare the ECDSA test by locking some funds at the validator address if there is none
prepTest ∷ Contract () (Maybe TransactionHash)
prepTest = do
  validator ← liftContractM "Caonnot get validator" getValidator
  let
    valHash = validatorHash validator

  netId ← getNetworkId
  valAddr ← liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos ← utxosAt valAddr

  if Map.isEmpty scriptUtxos then
    do
      let
        val = Value.lovelaceValueOf (BigInt.fromInt 1)

        lookups ∷ Lookups.ScriptLookups Void
        lookups = Lookups.validator validator

        constraints ∷ Constraints.TxConstraints Void Void
        constraints = Constraints.mustPayToScript valHash unitDatum
          Constraints.DatumInline
          val
      ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
      bsTx ← liftedE $ balanceTx ubTx
      sgTx <- signTransaction bsTx
      txId ← submit sgTx
      logInfo' $ ("Submitted ECDSA test prep tx: " <> show txId)
      awaitTxConfirmed txId
      logInfo' "Transaction confirmed."

      pure $ Just txId
  else
    pure Nothing

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification ∷ ECDSARed → Contract () TransactionHash
testVerification ecdsaRed = do
  let red = Redeemer $ toData ecdsaRed

  validator ← liftContractM "Can't get validator" getValidator
  let valHash = validatorHash validator

  netId ← getNetworkId
  valAddr ← liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos ← utxosAt valAddr
  txIn ← liftContractM "No UTxOs found at validator address"
    $ Set.findMin
    $ Map.keys scriptUtxos
  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs scriptUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput txIn red
  ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx ← liftedE $ balanceTx ubTx
  sgTx <- signTransaction bsTx
  txId ← submit sgTx
  logInfo' $ ("Submitted ECDSA test verification tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "Transaction confirmed."
  pure txId

-- | Testing ECDSA verification function on-chain
testECDSA ∷ Contract () TransactionHash
testECDSA = do
  privateKey <- liftEffect $ randomPrivateKey
  let
    publicKey = getECDSAPublicKey privateKey true
    message = unsafeCoerce privateKey
  messageHash <- liftAff $ sha256 message
  signature <- liftAff $ signECDSA messageHash privateKey false
  testVerification $
    -- | TODO: Find the correct format
    ECDSARed
      { msg: unsafeCoerce messageHash
      , sig: unsafeCoerce signature
      , pk: unsafeCoerce publicKey
      }

getValidator ∷ Maybe Validator
getValidator = do
  decodeTextEnvelope ecdsaScript >>= plutusScriptV2FromEnvelope >>> map wrap
