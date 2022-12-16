module Ctl.Examples.ECDSA (contract) where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Crypto.Secp256k1.ECDSA
  ( ECDSAPublicKey
  , ECDSASignature
  , MessageHash
  , deriveEcdsaSecp256k1PublicKey
  , signEcdsaSecp256k1
  )
import Contract.Crypto.Secp256k1.Utils
  ( hashMessageSha256
  , randomSecp256k1PrivateKey
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (byteArrayFromIntArrayUnsafe)
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

newtype ECDSARedemeer = ECDSARedemeer
  { msg :: MessageHash
  , sig :: ECDSASignature
  , pk :: ECDSAPublicKey
  }

derive instance Generic ECDSARedemeer _
derive instance Newtype ECDSARedemeer _

instance ToData ECDSARedemeer where
  toData (ECDSARedemeer { msg, sig, pk }) = Constr zero
    [ toData msg, toData sig, toData pk ]

contract :: Contract () Unit
contract = do
  txHash <- prepTest
  void $ testECDSA txHash

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
  txId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted ECDSA test preparation tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction confirmed: " <> show txId

  pure txId

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification
  :: TransactionHash -> ECDSARedemeer -> Contract () TransactionHash
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
  txId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Submitted ECDSA test verification tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction confirmed: " <> show txId
  pure txId

-- | Testing ECDSA verification function on-chain
testECDSA :: TransactionHash -> Contract () TransactionHash
testECDSA txHash = do
  privateKey <- liftEffect $ randomSecp256k1PrivateKey
  let
    publicKey = deriveEcdsaSecp256k1PublicKey privateKey
    message = byteArrayFromIntArrayUnsafe [ 0, 1, 2, 3 ]
  messageHash <- liftAff $ hashMessageSha256 message
  signature <- liftAff $ signEcdsaSecp256k1 privateKey messageHash
  testVerification txHash $
    ECDSARedemeer
      { msg: messageHash
      , sig: signature
      , pk: publicKey
      }

getValidator :: Maybe Validator
getValidator = do
  decodeTextEnvelope validateECDSA >>= plutusScriptV2FromEnvelope >>> map wrap

foreign import validateECDSA :: String
