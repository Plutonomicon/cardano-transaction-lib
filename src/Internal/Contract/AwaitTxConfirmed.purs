module Ctl.Internal.Contract.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  , isTxConfirmed
  ) where

import Prelude

import Contract.Monad (liftedE)
import Control.Monad.Reader.Class (asks)
import Control.Parallel (parOneOf)
import Ctl.Internal.BalanceTx.Sync (syncWalletWithTransaction)
import Ctl.Internal.Contract (getChainTip)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryBackend (getBlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Data.Either (either)
import Data.Maybe (isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds, Seconds, fromDuration)
import Data.Traversable (for_)
import Data.UInt as UInt
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

-- | Wait until a transaction with given hash is confirmed.
-- | Use `awaitTxConfirmedWithTimeout` if you want to limit the time of waiting.
-- | Will fail to confirm if the transaction includes no outputs.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmed :: TransactionHash -> Contract Unit
awaitTxConfirmed txHash = do
  { timeout } <- asks (_.timeParams >>> _.awaitTxConfirmed)
  awaitTxConfirmedWithTimeout timeout txHash

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in seconds for waiting.
-- | Throws an exception on timeout.
-- | Will fail to confirm if the transaction includes no outputs.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmedWithTimeout :: Seconds -> TransactionHash -> Contract Unit
awaitTxConfirmedWithTimeout timeoutSeconds txHash = do
  -- If timeout is infinity, do not use a timeout at all.
  if unwrap timeoutSeconds == infinity then void waitForConfirmation
  else do
    txConfirmed <- parOneOf [ waitForConfirmation, waitAndFail ]
    if txConfirmed then pure unit
    else liftEffect $ throw $
      "awaitTxConfirmedWithTimeout: timeout exceeded, Transaction not \
      \confirmed"
  where
  -- Wait until the timeout elapses and return false.
  waitAndFail :: Contract Boolean
  waitAndFail = do
    liftAff $ delay $ timeout
    pure false

  timeout :: Milliseconds
  timeout = fromDuration timeoutSeconds

  -- Try to find the transaction indefinitely, with a waiting period between
  -- each request.
  --
  -- Assumption is that the Tx has at least one output.
  --
  -- CTL backend (kupo) ensures that UTxO changes are propagated in the
  -- `QueryHandle`, because it uses querying for UTxOs to check for tx
  -- availability in the first place.
  --
  -- But blockfrost backend performs the check for Tx existence using tx-by-hash
  -- query, so we need to check for the utxos separately.
  waitForConfirmation :: Contract Boolean
  waitForConfirmation = do
    { delay: delayMs } <- asks (_.timeParams >>> _.awaitTxConfirmed)
    tryUntilTrue delayMs (doesTxExist txHash)
    confirmTxDelay <-
      asks _.backend <#> (getBlockfrostBackend >=> _.confirmTxDelay)
    isBlockfrost <- asks _.backend <#> getBlockfrostBackend >>> isJust
    when isBlockfrost do
      tryUntilTrue delayMs (utxosPresentForTxHash txHash)
      for_ confirmTxDelay (liftAff <<< delay <<< fromDuration)
    whenM
      ( asks $ _.synchronizationParams
          >>> _.syncWalletWithTransaction
          >>> _.beforeTxConfirmed
      )
      $ syncWalletWithTransaction txHash
    pure true

-- Perform the check until it returns true.
tryUntilTrue :: Milliseconds -> Contract Boolean -> Contract Unit
tryUntilTrue delayTime check = go
  where
  go = do
    res <- check
    unless res do
      liftAff (delay delayTime)
      go

-- | Check that UTxOs are present using `getUtxoByOref` function
utxosPresentForTxHash :: TransactionHash -> Contract Boolean
utxosPresentForTxHash txHash = do
  queryHandle <- getQueryHandle
  mbTxOutput <- liftedE $ liftAff $ queryHandle.getUtxoByOref
    -- Here we assume that the tx has at least one output.
    (TransactionInput { transactionId: txHash, index: UInt.fromInt 0 })
  pure $ isJust mbTxOutput

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in slots for waiting.
-- | Throws an exception on timeout.
-- | Will fail to confirm if the transaction includes no outputs.
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmedWithTimeoutSlots :: Int -> TransactionHash -> Contract Unit
awaitTxConfirmedWithTimeoutSlots timeoutSlots txHash = do
  { delay: delayMs } <- asks (_.timeParams >>> _.awaitTxConfirmed)
  limitSlot <- getCurrentSlot >>= addSlots timeoutSlots
  tryUntilTrue delayMs do
    checkSlotLimit limitSlot
    doesTxExist txHash
  tryUntilTrue delayMs do
    checkSlotLimit limitSlot
    utxosPresentForTxHash txHash
  whenM
    ( asks $ _.synchronizationParams
        >>> _.syncWalletWithTransaction
        >>> _.beforeTxConfirmed
    )
    $ syncWalletWithTransaction txHash
  where
  addSlots :: Int -> Slot -> Contract Slot
  addSlots n slot =
    maybe (liftEffect $ throw "Cannot determine next slot") (pure <<< wrap) $
      unwrap slot `BigNum.add` BigNum.fromInt n

  getCurrentSlot :: Contract Slot
  getCurrentSlot = do
    { delay: delayMs } <- asks $ _.timeParams >>> _.awaitTxConfirmed
    getChainTip >>= case _ of
      Chain.TipAtGenesis -> do
        liftAff $ delay delayMs
        getCurrentSlot
      Chain.Tip (Chain.ChainTip { slot }) -> pure slot

  checkSlotLimit :: Slot -> Contract Unit
  checkSlotLimit limitSlot = do
    slot <- getCurrentSlot
    when (slot >= limitSlot) do
      liftEffect $ throw $
        "awaitTxConfirmedWithTimeoutSlots: \
        \ timeout exceeded, Transaction not confirmed"

-- | Checks if a Tx is known to the query layer. It may still be unconfirmed.
doesTxExist :: TransactionHash -> Contract Boolean
doesTxExist txHash = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.doesTxExist txHash
    >>= either (liftEffect <<< throw <<< show) pure

-- | Check if a transaction is confirmed at the moment, i.e. if its UTxOs
-- | are available to spend.
-- | If you want to delay until a transaction is confirmed, use
-- | `awaitTxConfirmed` or its variants.
isTxConfirmed :: TransactionHash -> Contract Boolean
isTxConfirmed txHash = do
  exists <- doesTxExist txHash
  if exists then utxosPresentForTxHash txHash
  else pure false
