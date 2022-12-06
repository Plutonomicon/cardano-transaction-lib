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
  -- prepTest >>= traverse_ awaitTxConfirmed
  -- testSecpV1 >>= awaitTxConfirmed
  prepTest >>= traverse_ awaitTxConfirmed
  testSecpV4 >>= awaitTxConfirmed

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
testSecpV1 ∷ Contract () TransactionHash
testSecpV1 = do
  testVerification $
    -- | TODO: Find the correct format
    ECDSARed
      { msg: hexToByteArrayUnsafe
          "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
      , sig: hexToByteArrayUnsafe
          "6fbc0da8a7db783c2e8adb2262339a0cd1ca584307821c65a918b329de6ee131852b89eac6984065f59051aa05ba38a0733b93c0ea9163abb059bd5fd0b66475"
      , pk: hexToByteArrayUnsafe
          "048379352db5140c4a39137dd08c69193af732a7ceee3e6ff8cb07e37ce82e035579551734d7d9e4b6853d45c4a7846d5ef570e56f0353f83dad3b478a5f6699"
      }

-- | Testing ECDSA verification function on-chain
testSecpV4 ∷ Contract () TransactionHash
testSecpV4 = do
  testVerification $
    -- | TODO: Find the correct format
    ECDSARed
      { msg: hexToByteArrayUnsafe
          "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
      , sig: hexToByteArrayUnsafe
          "304402205fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b68575102202754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
      , pk: hexToByteArrayUnsafe
          "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0"
      }

getValidator ∷ Maybe Validator
getValidator = do
  decodeTextEnvelope ecdsaScript >>= plutusScriptV2FromEnvelope >>> map wrap
