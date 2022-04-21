--
-- This module demonstrates how the `QueryM` interface can be used to build a
-- transaction from scratch. It creates and balances an example transaction that
-- gets UTxOs from the user's wallet and sends two Ada back to the same wallet
-- address
--
-- * Prerequisites
--   - A Chromium-based browser
--
--   - A Nami wallet funded with test Ada ("tAda") and collateral set, If you need
--     tAda, visit https://testnets.cardano.org/en/testnets/cardano/tools/faucet/
--
-- * How to run
--
--   The `QueryM` interface requires several external services to be running. From
--   the repository root, run the following commands:
--
--   - `make run-testnet-node`
--      Starts a testnet Cardano node. May take some time to sync fully
--
--   - `make run-testnet-ogmios`
--      Starts the Ogmios service. Also needs to sync with the running node
--
--   - `make run-haskell-server`
--      Starts the external Haskell server that will perform the transaction
--      fee calculations (no sync required)
--
--   Once these services are *fully synced*, run:
--
--   - `npm run dev` and visit `localhost:4008`. You may be prompted to enable
--     access to your wallet if you have not run this example before. You will
--     also be prompted to sign the transaction using your Nami password

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
  , logInfo
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
import Data.Map as Map

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
    logInfo Map.empty $ "Tx ID: " <> show txId
