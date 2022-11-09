-- | Example contract that uses a single signer native script as a minting
-- | policy
module Ctl.Examples.NativeScriptMints (main, example, contract, pkhPolicy) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(NativeMintingPolicy)
  , NativeScript(ScriptPubkey)
  )
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Ctl.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , liftedHead
  , maybeArrayToHead
  , mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  ) as Helpers
import Data.BigInt (BigInt)
import Data.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.NativeScriptMints"

  pkh <- Helpers.liftedHead "Couldn't get own pkh" ownPaymentPubKeyHash

  mp /\ cs <- Helpers.mkCurrencySymbol <<< pure $ pkhPolicy pkh
  tn <- Helpers.mkTokenName "NSToken"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyUsingNativeScript
        (nsPolicy pkh)
        tn $ BigInt.fromInt 100

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmed txId
  logInfo' "Minted successfully"

  toSelfContract cs tn $ BigInt.fromInt 50

toSelfContract :: CurrencySymbol -> TokenName -> BigInt -> Contract () Unit
toSelfContract cs tn amount = do
  pkh <- Helpers.liftedHead "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- Helpers.maybeArrayToHead <$> ownStakePubKeyHash

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Helpers.mustPayToPubKeyStakeAddress pkh skh
      $ Value.singleton cs tn
      $ amount

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmed txId
  logInfo' $ "Moved " <> show (BigInt.fromInt 50) <> " to self successfully"

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

nsPolicy :: PaymentPubKeyHash -> NativeScript
nsPolicy = ScriptPubkey <<< unwrap <<< unwrap

pkhPolicy :: PaymentPubKeyHash -> MintingPolicy
pkhPolicy = NativeMintingPolicy <<< nsPolicy
