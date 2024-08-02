-- | This module demonstrates creating a UTxO whose datum is inline via the
-- | `Contract` interface. The `checkDatumIsInlineScript` only validates if the
-- | scripts own input was supplied with an inline datum matching the redeemer.
module Ctl.Examples.PlutusV2.InlineDatum
  ( main
  , example
  , checkDatumIsInlineScript
  , payToCheckDatumIsInline
  , payToCheckDatumIsInlineWrong
  , readFromCheckDatumIsInline
  , spendFromCheckDatumIsInline
  ) where

import Contract.Prelude

import Cardano.Types (Credential(ScriptHashCredential))
import Cardano.Types.BigNum as BigNum
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData(Integer), RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( OutputDatum(OutputDatum)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt as BigInt
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.PlutusV2.InlineDatum"
    validator <- checkDatumIsInlineScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToCheckDatumIsInline vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromCheckDatumIsInline vhash validator txId

plutusData :: PlutusData
plutusData = Integer $ BigInt.fromInt 31415927

payToCheckDatumIsInline :: ValidatorHash -> Contract TransactionHash
payToCheckDatumIsInline vhash = do
  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript vhash plutusData
        Constraints.DatumInline
        $ Value.lovelaceValueOf
        $ BigNum.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromCheckDatumIsInline
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromCheckDatumIsInline vhash validator txId = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  txInput <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      (fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _))
  let
    redeemer :: RedeemerDatum
    redeemer = RedeemerDatum plutusData

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutput txInput redeemer

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

payToCheckDatumIsInlineWrong :: ValidatorHash -> Contract TransactionHash
payToCheckDatumIsInlineWrong vhash = do
  let

    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript vhash plutusData
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigNum.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

readFromCheckDatumIsInline
  :: ValidatorHash
  -> TransactionHash
  -> Contract Unit
readFromCheckDatumIsInline vhash txId = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  output <- liftM
    ( error
        ( "The id "
            <> show txId
            <> " does not have output locked at: "
            <> show scriptAddress
        )
    )
    (snd <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _))
  (unwrap output).datum `shouldEqual` Just (OutputDatum plutusData)
  logInfo' "Successfully read inline datum."

  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

checkDatumIsInlineScript :: Contract Validator
checkDatumIsInlineScript = do
  liftMaybe (error "Error decoding checkDatumIsInline") do
    envelope <- decodeTextEnvelope checkDatumIsInline
    plutusScriptFromEnvelope envelope

foreign import checkDatumIsInline :: String
