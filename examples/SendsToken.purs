-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit transactions. It creates two transactions: one that
-- | mints a token and one that sends that token to the owner's address.

module Ctl.Examples.SendsToken (main, example, contract) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  ) as Helpers
import Data.Array (head)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SendsToken"

  mintToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully, Sending token to own address"

  sendToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"

mintToken :: Contract TransactionHash
mintToken = do
  mp /\ value <- tokenValue
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue value

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  submitTxFromConstraints lookups constraints

sendToken :: Contract TransactionHash
sendToken = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes
  _ /\ value <- tokenValue
  let
    constraints :: Constraints.TxConstraints
    constraints = Helpers.mustPayToPubKeyStakeAddress pkh skh value

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

tokenValue :: Contract (MintingPolicy /\ Value)
tokenValue = do
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "TheToken"
  pure $ mp /\ Value.singleton cs tn one
