-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module Examples.Pkh2PkhGero (main) where

import Contract.Prelude

import Contract.Address
  ( NetworkId(TestnetId)
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , LogLevel(Trace)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , liftedE
  , liftedM
  , logInfo'
  , mkContractConfig
  , runContract_
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (mkGeroWalletAff)
import Data.BigInt as BigInt
import Contract.Test.Examples (publishTestFeedback)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkGeroWalletAff
  cfg <- mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Trace
    , extraConfig: {}
    , wallet
    }

  runContract_ cfg $ do
    logInfo' "Running Examples.Pkh2PkhGero"
    pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
    skh <- liftedM "Failed to get own SKH" ownStakePubKeyHash

    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = Constraints.mustPayToPubKeyAddress pkh skh
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

      lookups :: Lookups.ScriptLookups Void
      lookups = mempty

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    bsTx <- liftedE $ balanceAndSignTxE ubTx
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId

    liftAff $ publishTestFeedback true
