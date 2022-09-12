module Examples.OneShotMinting
  ( contract
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Scripts (MintingPolicy, applyArgs)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction (TransactionInput, awaitTxConfirmed, plutusV1Script)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (singleton) as Value
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable) as Map
import Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  ) as Helpers

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.OneShotMinting"

  ownAddress <- liftedM "Failed to get own address" getWalletAddress
  (utxos :: Array _) <-
    Map.toUnfoldable <$> (liftedM "Failed to get utxo set" $ utxosAt ownAddress)
  oref <-
    liftContractM "Utxo set is empty" $ fst <$> Array.head utxos

  mp /\ cs <- Helpers.mkCurrencySymbol (oneShotMintingPolicy oref)
  tn <- Helpers.mkTokenName "CTLNFT"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValue (Value.singleton cs tn one)

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

foreign import oneShotMinting :: String

oneShotMintingPolicy :: TransactionInput -> Contract () MintingPolicy
oneShotMintingPolicy oref = do
  unappliedMintingPolicy <-
    map (wrap <<< plutusV1Script)
      (textEnvelopeBytes oneShotMinting PlutusScriptV1)
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)

  liftedE $ applyArgs unappliedMintingPolicy mintingPolicyArgs

