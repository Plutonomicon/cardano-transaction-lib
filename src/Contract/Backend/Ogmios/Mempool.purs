-- | A module for interacting with Ogmios' Local TX Monitor
-- | These functions only work with Ogmios backend (not Blockfrost!).
-- | https://ogmios.dev/mini-protocols/local-tx-monitor/
module Contract.Backend.Ogmios.Mempool
  ( module Ogmios
  , acquireMempoolSnapshot
  , fetchMempoolTxs
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  , withMempoolSnapshot
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Control.Monad.Error.Class (liftMaybe, try)
import Ctl.Internal.Cardano.Types.Transaction (Transaction)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.QueryM
  ( acquireMempoolSnapshot
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  ) as QueryM
import Ctl.Internal.QueryM.Ogmios
  ( MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , TxHash
  ) as Ogmios
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Data.Array as Array
import Data.List (List(Cons))
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Exception (error)

-- | Establish a connection with the Local TX Monitor.
-- | Instantly accquires the current mempool snapshot, and will wait for the next
-- | mempool snapshot if used again before using `releaseMempool`.
acquireMempoolSnapshot :: Contract Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshot = wrapQueryM QueryM.acquireMempoolSnapshot

-- | Check to see if a TxHash is present in the current mempool snapshot.
mempoolSnapshotHasTx
  :: Ogmios.MempoolSnapshotAcquired -> TransactionHash -> Contract Boolean
mempoolSnapshotHasTx ms = wrapQueryM <<< QueryM.mempoolSnapshotHasTx ms <<<
  unwrap

-- | Get the first received TX in the current mempool snapshot. This function can
-- | be recursively called to traverse the finger-tree of the mempool data set.
-- | This will return `Nothing` once it has reached the end of the current mempool.
mempoolSnapshotNextTx
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Maybe Transaction)
mempoolSnapshotNextTx mempoolAcquired = do
  mbTx <- wrapQueryM $ QueryM.mempoolSnapshotNextTx mempoolAcquired
  for mbTx \(Ogmios.MempoolTransaction { raw }) -> do
    byteArray <- liftMaybe (error "Failed to decode transaction")
      $ hexToByteArray raw
    liftMaybe (error "Failed to decode tx")
      $ hush
      $ deserializeTransaction
      $ wrap byteArray

-- | The acquired snapshot’s size (in bytes), number of transactions, and
-- | capacity (in bytes).
mempoolSnapshotSizeAndCapacity
  :: Ogmios.MempoolSnapshotAcquired -> Contract Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity = wrapQueryM <<<
  QueryM.mempoolSnapshotSizeAndCapacity

-- | Release the connection to the Local TX Monitor.
releaseMempool
  :: Ogmios.MempoolSnapshotAcquired -> Contract Unit
releaseMempool = wrapQueryM <<< QueryM.releaseMempool

-- | A bracket-style function for working with mempool snapshots - ensures
-- | release in the presence of exceptions
withMempoolSnapshot
  :: forall a
   . (Ogmios.MempoolSnapshotAcquired -> Contract a)
  -> Contract a
withMempoolSnapshot f = do
  s <- acquireMempoolSnapshot
  res <- try $ f s
  releaseMempool s
  liftEither res

-- | Recursively request the next TX in the mempool until Ogmios does not
-- | respond with a new TX.
fetchMempoolTxs
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Array Transaction)
fetchMempoolTxs ms = Array.fromFoldable <$> go
  where
  go = do
    nextTX <- mempoolSnapshotNextTx ms
    case nextTX of
      Just tx -> Cons tx <$> go
      Nothing -> pure mempty
