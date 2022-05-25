-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
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
import Effect.Aff (launchAff_)

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
    logInfo' "Running Examples.Pkh2Pkh"
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
