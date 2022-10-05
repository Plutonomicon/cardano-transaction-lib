module Ctl.Examples.IncludeDatum
  ( example
  , includeDatumScript
  , main
  , payToIncludeDatum
  , spendFromIncludeDatum
  )
  where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftedE)
import Contract.Numeric.Convert (uIntToBigInt)
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData (Datum(..), PlutusData(..), toData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope (TextEnvelopeType(PlutusScriptV1), textEnvelopeBytes)
import Contract.Transaction (TransactionHash, TransactionInput(TransactionInput), awaitTxConfirmed, balanceAndSignTxE, submit, plutusV1Script, lookupTxHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (_input)
import Ctl.Internal.Serialization.Types (BigInt)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Lens (view)
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.IncludeDatum"
    validator <- includeDatumScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToIncludeDatum vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromIncludeDatum vhash validator txId
  publishTestFeedback true

plutusData :: PlutusData
plutusData = Integer $ BigInt.fromInt 42

payToIncludeDatum :: ValidatorHash -> Contract () TransactionHash
payToIncludeDatum vhash = do
  let
    datum :: Datum
    datum = Datum plutusData
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash datum Constraints.DatumWitness $ Value.lovelaceValueOf $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
  buildBalanceSignAndSubmitTx lookups constraints

spendFromIncludeDatum
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromIncludeDatum vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress
  case view _input <$> head (lookupTxHash txId utxos) of
    Just txInput ->
      let
        datum :: Datum
        datum = Datum plutusData
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
--            <> Constraints.mustIncludeDatum datum
      in
        do
          spendTxId <- buildBalanceSignAndSubmitTx lookups constraints
          awaitTxConfirmed spendTxId
          logInfo' "Successfully spent locked values."
    _ -> logInfo' "no locked output at address"


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

foreign import includeDatum :: String

includeDatumScript :: Contract () Validator
includeDatumScript = wrap <<< plutusV1Script <$> textEnvelopeBytes
  includeDatum
  PlutusScriptV1

-- <> Constraints.mustIncludeDatum unitDatum
-- <> Constraints.mustHashDatum (mkDatumHash "1234") unitDatum
-- <> Constraints.mustHashDatum (Hashing.datumHash $ wrap plutusData') unitDatum

-- plutusData :: PlutusData
-- plutusData = Integer $ BigInt.fromInt 31415927

-- plutusData' :: PlutusData
-- plutusData' = Constr (BigInt.fromInt 0) []

-- datum :: Datum
-- datum = Datum plutusData

-- mkDatumHash :: String -> DataHash
-- mkDatumHash = wrap <<< hexToByteArrayUnsafe

--Hashing.datumHash (wrap $ Integer (fromInt 0))
-- datHash = case datumHash datum of
--   Just dh -> dh
--   Nothing -> throwError "error"
