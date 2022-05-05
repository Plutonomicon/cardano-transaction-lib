-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
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

module Examples.Pkh2Pkh (main) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId), ownPaymentPubKeyHash)
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , LogLevel(Trace)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , defaultSlotConfig
  , launchAff_
  , liftedE
  , liftedM
  , logInfo'
  , mkContractConfig
  , runContract_
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (mkNamiWalletAff)
import Data.BigInt as BigInt

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , slotConfig: defaultSlotConfig
    , logLevel: Trace
    , extraConfig: {}
    , wallet
    }

  runContract_ cfg $ do
    pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash

    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = Constraints.mustPayToPubKey pkh
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

      lookups :: Lookups.ScriptLookups Void
      lookups = mempty

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    BalancedSignedTransaction bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    txId <- submit bsTx.signedTxCbor
    logInfo' $ "Tx ID: " <> show txId
