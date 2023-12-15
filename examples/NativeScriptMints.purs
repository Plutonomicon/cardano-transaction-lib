-- | Example contract that uses a single signer native script as a minting
-- | policy
module Ctl.Examples.NativeScriptMints (main, example, contract, pkhPolicy) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  )
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(NativeMintingPolicy)
  , NativeScript(ScriptPubkey)
  )
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  , mustPayToPubKeyStakeAddress
  ) as Helpers
import Data.Array (head)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.NativeScriptMints"

  pkh <- liftedM "Couldn't get own pkh" $ head <$> ownPaymentPubKeyHashes

  mp /\ cs <- Helpers.mkCurrencySymbol <<< pure $ pkhPolicy pkh
  tn <- Helpers.mkTokenName "NSToken"

  let
    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintCurrencyUsingNativeScript
        (nsPolicy pkh)
        tn $ BigInt.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Minted successfully"

  toSelfContract cs tn $ BigInt.fromInt 50

toSelfContract :: CurrencySymbol -> TokenName -> BigInt -> Contract Unit
toSelfContract cs tn amount = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes

  let
    constraints :: Constraints.TxConstraints
    constraints = Helpers.mustPayToPubKeyStakeAddress pkh skh
      $ Value.singleton cs tn
      $ amount

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' $ "Moved " <> show (BigInt.fromInt 50) <> " to self successfully"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

nsPolicy :: PaymentPubKeyHash -> NativeScript
nsPolicy = ScriptPubkey <<< unwrap <<< unwrap

pkhPolicy :: PaymentPubKeyHash -> MintingPolicy
pkhPolicy = NativeMintingPolicy <<< nsPolicy
