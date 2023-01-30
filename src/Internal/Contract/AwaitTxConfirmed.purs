module Ctl.Internal.Contract.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  ) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Control.Parallel (parOneOf)
import Ctl.Internal.Contract (getChainTip)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryBackend (getBlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Contract.WaitUntilSlot (waitUntilSlot)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Transaction (TransactionHash)
import Data.Either (either)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds, Seconds(Seconds), fromDuration)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

awaitTxConfirmed :: TransactionHash -> Contract Unit
awaitTxConfirmed = awaitTxConfirmedWithTimeout (Seconds infinity)

-- NOTE: This function will always fail if the timeout is less than the value
-- of the Blockfrost `confirmTxDelay` parameter.
awaitTxConfirmedWithTimeout :: Seconds -> TransactionHash -> Contract Unit
awaitTxConfirmedWithTimeout timeoutSeconds txHash =
  -- If timeout is infinity, do not use a timeout at all.
  if unwrap timeoutSeconds == infinity then void findTx
  else do
    txFound <- parOneOf [ findTx, waitAndFail ]
    if txFound then pure unit
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
  -- If `confirmTxDelay` of `BlockfrostBackend` is set, wait the specified
  -- number of seconds after the transaction is confirmed, then check the
  -- transaction confirmation status again to handle possible rollbacks.
  -- We do this due to asynchronous updates across API endpoints, the delay
  -- should be enough time for the effects of the transaction to settle.
  findTx :: Contract Boolean
  findTx = do
    confirmTxDelay <-
      asks _.backend <#> (getBlockfrostBackend >=> _.confirmTxDelay)
    worker (fromDuration <$> confirmTxDelay) false
    where
    worker :: Maybe Milliseconds -> Boolean -> Contract Boolean
    worker confirmTxDelay foundBefore =
      isTxConfirmed txHash >>= case _ of
        -- Make sure that the transaction has not been rolled back after the
        -- confirmation delay.
        true | foundBefore ->
          pure true
        true ->
          confirmTxDelay #
            maybe (pure true) (\d -> liftAff (delay d) *> worker (Just d) true)
        false ->
          liftAff (delay delayTime) *> worker confirmTxDelay false
      where
      delayTime :: Milliseconds
      delayTime = wrap 1000.0

awaitTxConfirmedWithTimeoutSlots :: Int -> TransactionHash -> Contract Unit
awaitTxConfirmedWithTimeoutSlots timeoutSlots txHash =
  getCurrentSlot >>= addSlots timeoutSlots >>= go
  where
  getCurrentSlot :: Contract Slot
  getCurrentSlot = getChainTip >>= case _ of
    Chain.TipAtGenesis -> do
      liftAff $ delay $ wrap 1000.0
      getCurrentSlot
    Chain.Tip (Chain.ChainTip { slot }) -> pure slot

  addSlots :: Int -> Slot -> Contract Slot
  addSlots n slot =
    maybe (liftEffect $ throw "Cannot determine next slot") (pure <<< wrap) $
      unwrap slot `BigNum.add` BigNum.fromInt n

  go :: Slot -> Contract Unit
  go timeout =
    isTxConfirmed txHash >>= \found ->
      unless found do
        slot <- getCurrentSlot
        when (slot >= timeout) do
          liftEffect $ throw $
            "awaitTxConfirmedWithTimeoutSlots: \
            \ timeout exceeded, Transaction not confirmed"
        void $ addSlots 1 slot >>= waitUntilSlot
        go timeout

isTxConfirmed :: TransactionHash -> Contract Boolean
isTxConfirmed txHash = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.isTxConfirmed txHash
    >>= either (liftEffect <<< throw <<< show) pure

