module Test.Ctl.Testnet.SameWallets
  ( suite
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Testnet
  ( ContractTestPlan
  , sameWallets
  , withKeyWallet
  )
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Internal.Test.UtxoDistribution (InitialUTxOs)
import Data.Array as Array
import Mote (group, test)

suite :: ContractTestPlan
suite =
  let
    distribution :: InitialUTxOs /\ InitialUTxOs
    distribution =
      -- Alice
      [ BigNum.fromInt 1_000_000_000 ] /\
        -- Bob
        [ BigNum.fromInt 1_000_000_000 ]

    tokenNameAscii :: String
    tokenNameAscii = "CTLNFT"
  in
    sameWallets distribution do
      group "SameWallets" do
        test "Alice mints some tokens" \(alice /\ _) -> do
          tn <- Helpers.mkAssetName tokenNameAscii
          withKeyWallet alice $ void $ alwaysMint tn
        test "Alice sends a token to Bob" \(alice /\ bob) -> do
          bobPKH <- withKeyWallet bob do
            liftedM "Failed to get Bob's PKH"
              $ Array.head
              <$> ownPaymentPubKeyHashes
          withKeyWallet alice do
            mp <- alwaysMintsPolicy
            let cs = PlutusScript.hash mp
            tn <- Helpers.mkAssetName tokenNameAscii
            pkh2pkh bobPKH $ Value.singleton cs tn BigNum.one

alwaysMint :: TokenName -> Contract Unit
alwaysMint tn = do
  mp <- alwaysMintsPolicy
  let
    cs = PlutusScript.hash mp

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue
      $ Mint.singleton cs tn
      $ Int.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

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
