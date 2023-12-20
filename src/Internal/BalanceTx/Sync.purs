-- | Internal module that handles UTxO set synchronization between backend
-- | and wallet query layers.
module Ctl.Internal.BalanceTx.Sync
  ( syncBackendWithWallet
  , syncWalletWithTransaction
  , syncWalletWithTxInputs
  , isCip30Wallet
  , getControlledAddresses
  , withoutSync
  , disabledSynchronizationParams
  ) where

import Prelude

import Contract.Log (logError', logTrace', logWarn')
import Contract.Monad (Contract, liftedE)
import Control.Monad.Reader (local)
import Control.Monad.Reader.Class (asks)
import Control.Parallel (parOneOf, parTraverse, parallel, sequential)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.Transaction as Cardano
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Contract.Monad
  ( ContractSynchronizationParams
  , getQueryHandle
  )
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getUnusedAddresses
  , getWalletAddresses
  , getWalletCollateral
  , getWalletUtxos
  )
import Ctl.Internal.Helpers (liftEither, liftedM)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.Transaction (TransactionHash, TransactionInput)
import Ctl.Internal.Wallet (Wallet(GenericCip30))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds, fromDuration)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Effect.Ref as Ref

-- | Wait until all UTxOs that the wallet returns are visible in the
-- | query layer.
syncBackendWithWallet :: Contract Unit
syncBackendWithWallet = whenM isCip30Wallet do
  { delay: delayMs, timeout } <- asks $ _.timeParams >>> _.syncBackend
  { errorOnTimeout } <- asks $
    _.synchronizationParams >>> _.syncBackendWithWallet
  let
    errorMessage = "Failed to wait for wallet state synchronization (timeout). "
      <> guard (not errorOnTimeout) "Continuing anyway. "
      <> "Consider increasing `timeParams.syncBackend.timeout`"
      <> " in `ContractParams`"
  knownTxsRef <- asks (_.knownTxs >>> _.backend)
  let
    sync :: Contract Unit
    sync = do
      utxos <- getControlledUtxos
      alreadyFoundTxs <- liftEffect $ Ref.read knownTxsRef
      let
        isAlreadyFoundUtxo =
          flip Set.member alreadyFoundTxs <<< _.transactionId <<< unwrap

        -- all utxos from the wallet, except of those we have already seen
        notYetFoundUtxos :: Array TransactionInput
        notYetFoundUtxos =
          Array.filter (not <<< isAlreadyFoundUtxo)
            $ fst <$> Map.toUnfoldableUnordered utxos
      -- split the UTxOs into known to the query layer and not yet known
      { yes: newlyFound, no: stillNotFound } <- Array.partition (snd >>> isJust)
        <$> parTraverse (\input -> Tuple input <$> getUtxo' input)
          notYetFoundUtxos
      -- Mark newly found TxIds as known
      liftEffect $ Ref.modify_
        ( Set.union
            $ Set.fromFoldable
            $ newlyFound <#> fst >>> unwrap >>> _.transactionId
        )
        knownTxsRef
      if Array.null stillNotFound then do
        logTrace' "syncBackendWithWallet: synchronization finished"
      else do
        logTrace' $
          "syncBackendWithWallet: waiting for query layer state " <>
            "synchronization with the wallet..."
        liftAff (delay delayMs)
        sync
  parOneOf
    [ sync
    , waitAndError errorOnTimeout (fromDuration timeout) errorMessage
    ]

-- | Wait until the wallet can see the UTxOs created by a given transaction.
-- | This function assumes that the transaction has already been confirmed
-- | (i.e. the backend query layer can see its UTxOs).
-- | This function does not have any effect if `KeyWallet` is used.
-- | This function does not have any effect if none of the outputs are at
-- | addresses controlled by the wallet.
-- | The assumption is that the transaction has at least one output and that it
-- | hasn't been consumed by another transaction yet.
-- | You don't need to use this function if you use `awaitTxConfirmed*`.
-- |
-- | Theoretically, this function does not work well with UTxO locking feature
-- | of Eternl wallet, but in practice, there's a very low chance that the user
-- | will lock a UTxO right after submitting a Tx before we see it.
-- | Please read `doc/query-layers.md` for more context.
syncWalletWithTransaction :: TransactionHash -> Contract Unit
syncWalletWithTransaction txHash = whenM isCip30Wallet do
  { delay: delayMs, timeout } <- asks (_.timeParams >>> _.syncWallet)
  { errorOnTimeout } <- asks $
    _.synchronizationParams >>> _.syncWalletWithTransaction
  let
    errorMessage = "syncWalletWithTransaction: Failed to wait for wallet state "
      <> "synchronization. "
      <> guard (not errorOnTimeout) "Continuing anyway. "
      <> "This may indicate UTxO locking"
      <> " in use in the wallet. Consider increasing "
      <> "`timeParams.syncWallet.timeout` in `ContractParams`. "
      <> "See `doc/query-layers.md` for more info."
  queryHandle <- getQueryHandle
  let
    sync :: Contract Unit
    sync = do
      inputs <- map fst <<< Map.toUnfoldable <$> getControlledUtxos
      -- We can wait for just one, because once we have it, we know that
      -- the tx effects have propagated to the wallet (Txs are atomic)
      if Array.any (\input -> (unwrap input).transactionId == txHash) inputs then
        do
          logTrace' "syncWalletWithTransaction: synchronization finished"
      else do
        logTrace' $
          "syncWalletWithTransaction: waiting for wallet state synchronization "
            <> "with the query layer, querying for Tx: "
            <> byteArrayToHex (unwrap txHash)
        liftAff (delay delayMs)
        sync
  -- Collect all the addresses controlled by the wallet
  -- (reward addresses are omitted on purpose, we don't need them)
  ownAddresses /\ outputAddresses <- sequential do
    Tuple <$> parallel getControlledAddresses <*> parallel do
      liftAff $ liftEither =<< do
        queryHandle.getOutputAddressesByTxHash txHash <#>
          bimap (error <<< show) Set.fromFoldable
  if Set.isEmpty (Set.intersection ownAddresses outputAddresses) then do
    logWarn' $ "syncWalletWithTransaction: "
      <> "Skipping wait for wallet state synchronization, because the "
      <> "transaction does not include outputs controlled by this wallet."
    logTrace' $
      "Wallet addresses: " <> show ownAddresses
    logTrace' $
      "Transaction output addresses: " <> show ownAddresses
  else do
    parOneOf
      [ sync
      , waitAndError errorOnTimeout (fromDuration timeout) errorMessage
      ]

-- | Waits until all provided transaction inputs appear in the UTxO
-- | set provided by the wallet.
-- | This is a hacky solution to the problem of wallets not seeing UTxOs that
-- | hasn't been fully confirmed at the moment of a `sign()` call.
-- | The problem is, since the wallet can't detect UTxO origin, it can't decide
-- | which of the private keys to use for signing.
-- | As a result, we get `MissingVKeyWitnesses`.
-- | You don't have to call this function before `signTx`, because its use is
-- | enabled by default.
syncWalletWithTxInputs :: Array TransactionInput -> Contract Unit
syncWalletWithTxInputs txInputs = whenM isCip30Wallet do
  { delay: delayMs, timeout } <- asks (_.timeParams >>> _.syncWallet)
  { errorOnTimeout } <- asks $
    _.synchronizationParams >>> _.syncWalletWithTxInputs
  ownAddrs <- getControlledAddresses
  ownInputUtxos <- txInputs #
    parTraverse
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
    "syncWalletWithTxInputs: waiting for UTxO set to synchronize with the "
      <> "following inputs: "
      <> show ownInputUtxos
  let
    sync = do
      walletUtxos <- getControlledUtxos
      let difference = ownInputUtxos `Map.difference` walletUtxos
      if Map.isEmpty difference then do
        logTrace' "syncWalletWithTxInputs: synchronization finished"
      else do
        logTrace' $ "syncWalletWithTxInputs: remaining UTxOs that the wallet "
          <> "does not know about: "
          <> show difference
        liftAff $ delay delayMs
        sync
  parOneOf
    [ sync, waitAndError errorOnTimeout (fromDuration timeout) errorMessage ]
  where
  errorMessage =
    "syncWalletWithTxInputs: timeout while waiting for wallet"
      <> " UTxO set and CTL query layer UTxO set to synchronize "
      <> "(see `timeParams.syncWallet.timeout` in `ContractParams`)"

-- | A helper to set `synchronizationParams` to `disabledSynchronizationParams`
-- | locally, thus skipping the synchronization process during execution of
-- | the `Contract`.
withoutSync :: forall (a :: Type). Contract a -> Contract a
withoutSync = do
  local _ { synchronizationParams = disabledSynchronizationParams }

-- | Synchronization parameters that make all synchronization primitives
-- | a no-op.
-- | See `doc/query-layers.md` for more info.
disabledSynchronizationParams :: ContractSynchronizationParams
disabledSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false
      , beforeCip30Methods: false
      , beforeBalancing: false
      }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: false }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: false }
  }

-- | A version without plutus conversion for internal use.
getUtxo' :: TransactionInput -> Contract (Maybe Cardano.TransactionOutput)
getUtxo' oref = do
  queryHandle <- getQueryHandle
  liftedE $ liftAff $ queryHandle.getUtxoByOref oref

-- | Get all addresses contolled by a wallet:
-- | `getUsedAddresses`, `getUnusedAddresses` and `getChangeAddress` combined.
-- | Reward addresses are not included.
getControlledAddresses :: Contract (Set Address)
getControlledAddresses = do
  sequential $ combine
    <$> parallel (getWalletAddresses <#> Set.fromFoldable)
    <*> parallel (getUnusedAddresses <#> Set.fromFoldable)
    <*> parallel (getChangeAddress <#> Set.singleton)
  where
  combine used unused change = used `Set.union` unused `Set.union` change

getControlledUtxos :: Contract UtxoMap
getControlledUtxos = do
  sequential $ Map.union
    <$> parallel (getWalletCollateral <#> maybe Map.empty toUtxoMap)
    <*> parallel (getWalletUtxos <#> fromMaybe Map.empty)
  where
  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)

isCip30Wallet :: Contract Boolean
isCip30Wallet = asks $ _.wallet >>> case _ of
  Just (GenericCip30 _) -> true
  _ -> false

-- | If the first argument is true, it will throw. Otherwise, that would be a
-- | console.error call.
waitAndError :: Boolean -> Milliseconds -> String -> Contract Unit
waitAndError errorOnTimeout timeout errorMessage = do
  liftAff $ delay timeout
  if errorOnTimeout then liftEffect $ throw errorMessage
  else logError' errorMessage
