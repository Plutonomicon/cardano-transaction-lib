module QueryM.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (maybe)
import Data.Newtype (wrap, unwrap)
import Data.Number (infinity)
import Data.Time.Duration (Seconds(Seconds))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (now)
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
awaitTxConfirmedWithTimeout timeoutSeconds txHash = do
  nowMs <- getNowMs
  let timeoutTime = nowMs + unwrap timeoutSeconds * 1000.0
  go timeoutTime
  where
  getNowMs :: QueryM Number
  getNowMs = unwrap <<< unInstant <$> liftEffect now

  go :: Number -> QueryM Unit
  go timeoutTime = do
    isTxFound <- unwrap <$> mkDatumCacheRequest getTxByHash _.getTxByHash
      txHash
    nowMs <- getNowMs
    when (nowMs >= timeoutTime) do
      liftEffect $ throw $
        "awaitTxConfirmedWithTimeout: timeout exceeded, Transaction not \
        \confirmed"
    liftAff $ delay $ wrap 1000.0
    if isTxFound then pure unit else go timeoutTime

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
      case unwrap found of
        true -> pure unit
        false -> do
          slot <- getCurrentSlot
          when (slot >= timeout) do
            liftEffect $ throw $
              "awaitTxConfirmedWithTimeoutSlots: \
              \ timeout exceeded, Transaction not confirmed"
          void $ addSlots 1 slot >>= waitUntilSlot
          go timeout

