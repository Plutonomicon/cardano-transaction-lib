module Test.Ctl.Plutip.SameWallets
  ( suite
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (PlutipTestPlan, sameWallets, withKeyWallet)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers
import Ctl.Internal.Test.UtxoDistribution (InitialUTxOs)
import Data.Array as Array
import JS.BigInt as BigInt
import Mote (group, test)

suite :: PlutipTestPlan
suite =
  let
    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution =
      -- Alice
      [ BigInt.fromInt 1_000_000_000 ] /\
        -- Bob
        [ BigInt.fromInt 1_000_000_000 ]

    tokenNameAscii :: String
    tokenNameAscii = "CTLNFT"
  in
    sameWallets distribution do
      group "SameWallets" do
        test "Alice mints some tokens" \(alice /\ _) -> do
          tn <- Helpers.mkTokenName tokenNameAscii
          withKeyWallet alice $ void $ alwaysMint tn
        test "Alice sends a token to Bob" \(alice /\ bob) -> do
          bobPKH <- withKeyWallet bob do
            liftedM "Failed to get Bob's PKH"
              $ Array.head
              <$> ownPaymentPubKeyHashes
          withKeyWallet alice do
            cs <- Value.scriptCurrencySymbol <$> alwaysMintsPolicy
            tn <- Helpers.mkTokenName tokenNameAscii
            pkh2pkh bobPKH $ Value.singleton cs tn one

alwaysMint :: TokenName -> Contract Unit
alwaysMint tn = do
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue
      $ Value.singleton cs tn
      $ BigInt.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  submitTxFromConstraints lookups constraints
    >>= awaitTxConfirmed

pkh2pkh :: PaymentPubKeyHash -> Value -> Contract Unit
pkh2pkh recipient val = do

  pkh <-
    liftedM "Failed to get own PKH"
      $ Array.head
      <$> ownPaymentPubKeyHashes

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustPayToPubKey recipient val

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.ownPaymentPubKeyHash pkh

  submitTxFromConstraints lookups constraints
    >>= awaitTxConfirmed
