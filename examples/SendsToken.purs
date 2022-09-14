-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit transactions. It creates two transactions: one that 
-- | mints a token and one that sends that token to the owner's address.

module CTL.Examples.SendsToken (main, example, contract) where

import CTL.Contract.Prelude

import CTL.Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, launchAff_, liftedM, runContract)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (MintingPolicy)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.Transaction (TransactionHash, awaitTxConfirmed)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value (Value)
import CTL.Contract.Value as Value
import CTL.Examples.AlwaysMints (alwaysMintsPolicy)
import CTL.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  ) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.SendsToken"

  mintToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully, Sending token to own address"

  sendToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"

mintToken :: Contract () TransactionHash
mintToken = do
  mp /\ value <- tokenValue
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValue value

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

sendToken :: Contract () TransactionHash
sendToken = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  _ /\ value <- tokenValue
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Helpers.mustPayToPubKeyStakeAddress pkh skh value

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

tokenValue :: Contract () (MintingPolicy /\ Value)
tokenValue = do
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "TheToken"
  pure $ mp /\ Value.singleton cs tn one
