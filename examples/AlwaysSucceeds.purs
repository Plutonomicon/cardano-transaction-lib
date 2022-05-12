-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
-- |
-- | * Prerequisites
-- |   - A Chromium-based browser (for Nami compatibility)
--
-- |   - A Nami wallet funded with test Ada ("tAda") and collateral set, If you need
-- |     tAda, visit https://testnets.cardano.org/en/testnets/cardano/tools/faucet/
--
-- | * How to run
--
-- |   The `Contract` interface requires several external services to be running.
-- |   From the repository root, run `nix run .#ctl-runtime` to launch all
-- |   required services
--
-- |   Once all of the services are *fully synced*, run:
--
-- |   - `make run-dev` and visit `localhost:4008`. You may be prompted to enable
-- |     access to your wallet if you have not run this example before. You will
-- |     also be prompted to sign the transaction using your Nami password

module Examples.AlwaysSucceeds (main) where

import Contract.Prelude

import Contract.Monad
  ( ContractConfig(ContractConfig)
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , logInfo'
  , runContract_
  , traceContractConfig
  )
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (mkNamiWalletAff)
import Data.Argonaut (decodeJson, fromString)
import Data.BigInt as BigInt

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    validator <- liftContractM "Invalid script JSON" $ alwaysSucceedsScript
    vhash <- liftedM "Couldn't hash validator" $ validatorHash validator

    let
      -- Note that CTL does not have explicit equivalents of Plutus'
      -- `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
      -- of a "current" script. Thus, we have the single constraint
      -- `mustPayToScript`, and all scripts must be explicitly provided to build
      -- the transaction (see the value for `lookups` below as well)
      constraints :: Constraints.TxConstraints Unit Unit
      constraints = Constraints.mustPayToScript vhash unitDatum
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

      lookups :: Lookups.ScriptLookups PlutusData
      lookups = Lookups.validator validator

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    BalancedSignedTransaction bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    txId <- submit bsTx.signedTxCbor
    logInfo' $ "Tx ID: " <> show txId

alwaysSucceedsScript :: Maybe Validator
alwaysSucceedsScript = map wrap $ hush $ decodeJson $ fromString
  "4d01000033222220051200120011"
