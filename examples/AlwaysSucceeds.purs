-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address.
module Ctl.Examples.AlwaysSucceeds
  ( alwaysSucceedsScript
  , contract
  , example
  , main
  , payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  ) where

import Contract.Prelude

import Cardano.Types
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , PlutusScript
  , ScriptHash
  , TransactionHash
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as Script
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( _input
  , awaitTxConfirmed
  , lookupTxHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet (ownStakePubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Lens (view)
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.AlwaysSucceeds"
  validator <- alwaysSucceedsScript
  let vhash = Script.hash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

payToAlwaysSucceeds :: ScriptHash -> Contract TransactionHash
payToAlwaysSucceeds vhash = do
  -- Send to own stake credential. This is used to test mustPayToScriptAddress.
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  let
    value = Value.lovelaceValueOf $ BigNum.fromInt 2_000_000

    constraints :: TxConstraints
    constraints =
      case mbStakeKeyHash of
        Nothing ->
          Constraints.mustPayToScript vhash PlutusData.unit
            Constraints.DatumWitness
            value
        Just stakeKeyHash ->
          Constraints.mustPayToScriptAddress vhash
            (PubKeyHashCredential $ unwrap stakeKeyHash)
            PlutusData.unit
            Constraints.DatumWitness
            value

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromAlwaysSucceeds
  :: ScriptHash
  -> PlutusScript
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysSucceeds vhash validator txId = do
  -- Use own stake credential if available
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress
    (wrap $ ScriptHashCredential vhash)
    (wrap <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
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
      (view _input <$> head (lookupTxHash txId utxos))
  let
    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.validator validator
      , Lookups.unspentOutputs utxos
      , Lookups.datum PlutusData.unit
      ]

    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutput txInput unitRedeemer
  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

alwaysSucceedsScript :: Contract PlutusScript
alwaysSucceedsScript = do
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceeds
    plutusScriptFromEnvelope envelope

foreign import alwaysSucceeds :: String
