module Test.Ctl.Utils.DrainWallets (main) where

import Prelude

import Contract.Config
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (runContract)
import Contract.ScriptLookups (unspentOutputs)
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (mustBeSignedBy, mustSpendPubKeyOutput)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (utxosAt)
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , privateKeysToKeyWallet
  , withKeyWallet
  )
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array (head)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Node.Process (argv, exit)

-- Run with `spago run --main Test.Ctl.Utils.DrainWallets --exec-args "PRIVATE_PAYMENT_FILE WALLETS_DIR"`
main :: Effect Unit
main = do
  argv >>= case _ of
    [ _, privateKey, walletsDir ] ->
      launchAff_ $ run privateKey walletsDir
    _ -> do
      log $ joinWith "\n"
        [ "Wrong number of parameters provided."
        , "Usage:"
        , "  spago run --main Test.Ctl.Utils.DrainWallets --exec-args \"PRIVATE_PAYMENT_FILE WALLETS_DIR\""
        , ""
        , "  PRIVATE_PAYMENT_FILE - PaymentSigningKeyShelley_ed25519 file, as produced by cardano-cli"
        , "  WALLETS_DIR          - A directory of wallets"
        ]
      exit 1

run :: String -> String -> Aff Unit
run privateKey walletsDir = runContract config do
  walletFolders <- liftAff $ readdir walletsDir

  wallets <- liftAff $ for walletFolders \walletFolder -> do
    payment <- privatePaymentKeyFromFile $ Path.concat
      [ walletsDir, walletFolder, "payment_signing_key" ]
    mbStake <- hush <$> try
      ( privateStakeKeyFromFile $ Path.concat
          [ walletsDir, walletFolder, "stake_signing_key" ]
      )
    pure $ privateKeysToKeyWallet payment mbStake Nothing

  let
    merge r =
      { utxos: Map.unions (_.utxos <$> r)
      , usedWallets: foldMap _.usedWallets r
      }

  -- parTraverse breaks kupo
  { utxos, usedWallets } <- merge <$> for wallets \wallet -> withKeyWallet
    wallet
    do
      address <- getWalletAddresses >>= head >>> liftMaybe
        (error "no addresses")
      pkh <- ownPaymentPubKeyHashes >>= head >>> liftMaybe (error "no PKH")
      utxos <- utxosAt address
      pure
        { utxos
        , usedWallets: if Map.isEmpty utxos then [] else [ { wallet, pkh } ]
        }

  log $ joinWith " "
    [ "Checked", show $ Array.length wallets, "wallets." ]

  when (Map.isEmpty utxos) do
    log "No UTxOs to spend, nothing to do."
    liftEffect $ exit 0

  log $ joinWith " "
    [ "Found"
    , show $ Map.size utxos
    , "UTxOs in"
    , show $ Array.length usedWallets
    , "wallets."
    ]

  let
    constraints = foldMap mustSpendPubKeyOutput (Map.keys utxos)
      <> foldMap (_.pkh >>> mustBeSignedBy) usedWallets
    lookups = unspentOutputs utxos

  unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints

  balancedTx <- balanceTx unbalancedTx usedUtxos mempty
  balancedSignedTx <- Array.foldM
    (\tx wallet -> withKeyWallet wallet $ signTransaction tx)
    (wrap $ unwrap balancedTx)
    (_.wallet <$> usedWallets)
  txHash <- submit balancedSignedTx
  awaitTxConfirmed txHash
  where
  config =
    testnetConfig
      { walletSpec = pure $ UseKeys
          (PrivatePaymentKeyFile privateKey)
          Nothing
          Nothing
      , backendParams = mkCtlBackendParams
          { ogmiosConfig: defaultOgmiosWsConfig
          , kupoConfig:
              { port: UInt.fromInt 1442
              , host: "localhost"
              , secure: false
              , path: Nothing
              }
          }
      }
