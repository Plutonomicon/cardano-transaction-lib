-- | Internal module that handles UTxO set synchronization between backend
-- | and wallet query layers.
module Ctl.Internal.BalanceTx.Sync
  ( syncBackendWithWallet
  , syncWalletWithTransaction
  , syncWalletWithInputs
  , isCip30Wallet
  ) where

import Prelude

import Contract.Log (logError', logTrace', logWarn')
import Contract.Monad (Contract, liftedE)
import Control.Monad.Reader.Class (asks)
import Control.Parallel (parOneOf)
import Ctl.Internal.Cardano.Types.Transaction as Cardano
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getUnusedAddresses
  , getWalletAddresses
  , getWalletUtxos
  )
import Ctl.Internal.Contract.Wallet as Utxos
import Ctl.Internal.Helpers (liftEither, liftedM)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Ctl.Internal.Wallet (cip30Wallet)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (all)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)

-- | Wait until all UTxOs that the wallet returns are visible in the
-- | query layer.
syncBackendWithWallet :: Contract Unit
syncBackendWithWallet = whenM isCip30Wallet do
  { delay: delayMs, timeout } <- asks (_.timeParams >>> _.syncBackend)
  parOneOf [ sync delayMs, waitAndLogError timeout errorMessage ]
  where
  sync :: Milliseconds -> Contract Unit
  sync delayMs = do
    utxos <- getWalletUtxos <#> fromMaybe Map.empty
    allFound <- all isJust <$> traverse getUtxo'
      (fst <$> Map.toUnfoldableUnordered utxos :: Array TransactionInput)
    if allFound then do
      logTrace' "syncBackendWithWallet: synchronization finished"
    else do
      logTrace' $
        "syncBackendWithWallet: waiting for query layer state " <>
          "synchronization with the wallet..."
      liftAff (delay delayMs)
      sync delayMs
  errorMessage = "Failed to wait for wallet state synchronization (timeout). "
    <> "Continuing anyway. Consider increasing `timeParams.syncBackend.timeout`"
    <> " in `ContractParams`"

-- | Wait until the wallet can see the UTxOs created by a given transaction.
-- | This function assumes that the transaction has already been confirmed
-- | (i.e. the backend query layer can see its UTxOs).
-- | This function does not have any effect if `KeyWallet` is used.
-- | This function does not have any effect if none of the outputs are at
-- | addresses controlled by the wallet.
-- | You don't need to use this function if you use `awaitTxConfirmed*`.
syncWalletWithTransaction :: TransactionHash -> Contract Unit
syncWalletWithTransaction txHash = whenM isCip30Wallet do
  { delay: delayMs, timeout } <- asks (_.timeParams >>> _.syncWallet)
  queryHandle <- getQueryHandle
  -- Collect all the addresses controlled by the wallet
  -- (reward addresses are omitted on purpose, we don't need them)
  ownAddresses <- getControlledAddresses
  outputAddresses <- liftAff $ liftEither =<< do
    queryHandle.getOutputAddressesByTxHash txHash <#>
      bimap (error <<< show) Set.fromFoldable
  if Set.isEmpty (Set.intersection ownAddresses outputAddresses) then do
    logWarn' $
      "Skipping wait for wallet state synchronization, because the " <>
        "transaction does not include outputs controlled by this wallet."
    logTrace' $
      "Wallet addresses: " <> show ownAddresses
    logTrace' $
      "Transaction output addresses: " <> show ownAddresses
  else do
    parOneOf [ sync delayMs, waitAndLogError timeout errorMessage ]
  where
  sync :: Milliseconds -> Contract Unit
  sync delayMs = do
    inputs <- map fst <<< Map.toUnfoldable <<< fromMaybe Map.empty <$>
      getWalletUtxos
    if Array.any (\input -> (unwrap input).transactionId == txHash) inputs then
      do
        logTrace' "syncWalletWithTransaction: synchronization finished"
        pure unit
    else do
      logTrace' $
        "syncWalletWithTransaction: waiting for wallet state synchronization "
          <> "with the query layer, querying for Tx: "
          <> byteArrayToHex (unwrap txHash)
      liftAff (delay delayMs)
      sync delayMs
  errorMessage =
    "syncWalletWithTransaction: Failed to wait for wallet state "
      <> "synchronization. Continuing anyway. This may indicate UTxO locking"
      <> " in use in the wallet. Consider increasing "
      <> "`timeParams.syncWallet.timeout` in `ContractParams`"

-- | Waits untill all provided transaction inputs appear in the UTxO
-- | set provided by the wallet.
-- | This is a hacky solution to the problem of wallets not seeing UTxOs that
-- | hasn't been fully confirmed at the moment of a `sign()` call.
-- | Since it can't detect UTxO origin, it can't decide which of the private
-- | keys to use for signing. As a result, we get `MissingVKeyWitnesses`.
syncWalletWithInputs :: Array TransactionInput -> Contract Unit
syncWalletWithInputs txInputs = do
  { delay: delayMs, timeout } <- asks (_.timeParams >>> _.syncWallet)
  ownAddrs <- getControlledAddresses
  ownInputUtxos <- txInputs #
    traverse
      ( \txInput -> do
          utxo <- liftedM (error "Could not get utxo") $ getUtxo' txInput
          pure (txInput /\ utxo)
      ) >>> map
      ( Map.fromFoldable >>> Map.filter
          ( flip Set.member ownAddrs
              <<< _.address
              <<< unwrap
          )
      )
  logTrace' $
    "syncWalletWithInputs: waiting for UTxO set to synchronize with the "
      <> "following inputs: "
      <> show ownInputUtxos
  let
    go = do
      walletUtxos <- Utxos.getWalletUtxos <#> fromMaybe Map.empty
      let difference = ownInputUtxos `Map.difference` walletUtxos
      if Map.isEmpty difference then do
        logTrace' "syncWalletWithInputs: synchronization finished"
      else do
        liftAff $ delay delayMs
        logTrace' $ "syncWalletWithInputs: remaining UTxOs that the wallet "
          <> "does not know about: "
          <> show difference
        go
  parOneOf [ go, waitAndLogError timeout errorMessage ]
  where
  errorMessage =
    "syncWalletWithInputs: timeout while waiting for wallet"
      <> " UTxO set and CTL query layer UTxO set to synchronize "
      <> "(see `timeParams.syncWallet.timeout` in `ContractParams`)"

-- | Without plutus conversion
getUtxo' :: TransactionInput -> Contract (Maybe Cardano.TransactionOutput)
getUtxo' oref = do
  queryHandle <- getQueryHandle
  liftedE $ liftAff $ queryHandle.getUtxoByOref oref

getControlledAddresses :: Contract (Set Address)
getControlledAddresses = do
  used <- getWalletAddresses <#> Set.fromFoldable
  unused <- getUnusedAddresses <#> Set.fromFoldable
  change <- getChangeAddress <#> Set.fromFoldable
  pure $ used `Set.union` unused `Set.union` change

isCip30Wallet :: Contract Boolean
isCip30Wallet = asks $ isJust <<< (cip30Wallet <=< _.wallet)

waitAndLogError :: Milliseconds -> String -> Contract Unit
waitAndLogError timeout errorMessage = do
  liftAff $ delay timeout
  logError' errorMessage
