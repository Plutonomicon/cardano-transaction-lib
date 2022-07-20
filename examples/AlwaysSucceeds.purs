-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Examples.AlwaysSucceeds (main) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractAffM
  , liftedE
  , logInfo'
  , runContract_
  , traceTestnetContractConfig
  )
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  , balanceAndSignTxE
  , submit
  , plutusV1Script
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import Effect.Aff (delay)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- traceTestnetContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.AlwaysSucceeds"
    validator <- alwaysSucceedsScript
    vhash <- liftContractAffM "Couldn't hash validator"
      $ validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToAlwaysSucceeds vhash
    -- If the wallet is cold, you need a high parameter here.
    countToZero 60
    logInfo' "Try to spend locked values"
    spendFromAlwaysSucceeds vhash validator txId

countToZero :: Int -> Contract () Unit
countToZero n =
  unless (n <= 0) do
    logInfo' $ "Waiting before we try to unlock: " <> show n
    (liftAff <<< delay <<< wrap) 1000.0
    countToZero (n - 1)

payToAlwaysSucceeds :: ValidatorHash -> Contract () TransactionHash
payToAlwaysSucceeds vhash = do
  let
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash unitDatum
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromAlwaysSucceeds vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _) of
    Just txInput ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
      in
        void $ buildBalanceSignAndSubmitTx lookups constraints
    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at: "
        <> show scriptAddress
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ balanceAndSignTxE ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

foreign import alwaysSucceeds :: String

alwaysSucceedsScript :: Contract () Validator
alwaysSucceedsScript = wrap <<< plutusV1Script <$> textEnvelopeBytes alwaysSucceeds
  PlutusScriptV1
