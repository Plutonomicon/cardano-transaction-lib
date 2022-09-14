module QueryM.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  ) where

import Prelude

import Control.Parallel (parOneOf)
import Data.Maybe (isJust, maybe)
import Data.Newtype (wrap, unwrap)
import Data.Number (infinity)
import Data.Time.Duration (Seconds(Seconds), Milliseconds, fromDuration)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import QueryM (QueryM, getChainTip, mkDatumCacheRequest)
import QueryM.DatumCacheWsp (getTxByHash)
import QueryM.Ogmios (TxHash)
import QueryM.WaitUntilSlot (waitUntilSlot)
import Serialization.Address (Slot)
import Types.BigNum as BigNum
import Types.Chain as Chain

awaitTxConfirmed :: TxHash -> QueryM Unit
awaitTxConfirmed = awaitTxConfirmedWithTimeout (Seconds infinity)

awaitTxConfirmedWithTimeout :: Seconds -> TxHash -> QueryM Unit
awaitTxConfirmedWithTimeout timeoutSeconds txHash =
  let
    timeout :: Milliseconds
    timeout = fromDuration timeoutSeconds

    delayTime :: Milliseconds
    delayTime = wrap 1000.0
  -- If timeout is infinity, do not use a timeout at all
  in
    if unwrap timeoutSeconds == infinity then findTx delayTime
    else parOneOf
      [ findTx delayTime
      , waitAndFail timeout
      ]
  where
  -- Try to find the TX indefinitely, with a waiting period between each
  -- request
  findTx :: Milliseconds -> QueryM Unit
  findTx delayTime = do
    isTxFound <- isJust <<< unwrap <$> mkDatumCacheRequest getTxByHash
      _.getTxByHash
      txHash
    if isTxFound then pure unit
    else liftAff (delay delayTime) *> findTx delayTime

  -- Wait until the timeout elapses and fail
  waitAndFail :: Milliseconds -> QueryM Unit
  waitAndFail timeout = do
    liftAff $ delay $ timeout
    liftEffect $ throw $
      "awaitTxConfirmedWithTimeout: timeout exceeded, Transaction not \
      \confirmed"

awaitTxConfirmedWithTimeoutSlots :: Int -> TxHash -> QueryM Unit
awaitTxConfirmedWithTimeoutSlots timeoutSlots txHash = do
  getCurrentSlot >>= addSlots timeoutSlots >>= go

  where
  getCurrentSlot :: QueryM Slot
  getCurrentSlot = getChainTip >>= case _ of
    Chain.TipAtGenesis -> do
      liftAff $ delay $ wrap 1000.0
      getCurrentSlot
    Chain.Tip (Chain.ChainTip { slot }) -> pure slot

  addSlots :: Int -> Slot -> QueryM Slot
  addSlots n slot =
    maybe (liftEffect $ throw "Cannot determine next slot") (pure <<< wrap) $
      unwrap slot `BigNum.add` BigNum.fromInt n

  go :: Slot -> QueryM Unit
  go timeout =
    mkDatumCacheRequest getTxByHash _.getTxByHash txHash >>= \found ->
      unless (isJust $ unwrap found) do
        slot <- getCurrentSlot
        when (slot >= timeout) do
          liftEffect $ throw $
            "awaitTxConfirmedWithTimeoutSlots: \
            \ timeout exceeded, Transaction not confirmed"
        void $ addSlots 1 slot >>= waitUntilSlot
        go timeout
