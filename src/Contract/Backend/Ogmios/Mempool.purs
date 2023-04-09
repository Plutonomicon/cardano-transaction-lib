-- A module for interacting with Ogmios' Local TX Monitor
module Contract.Backend.Ogmios.Mempool
  ( module Ogmios
  , acquireMempoolSnapshot
  , fetchMempoolTXs
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , mempoolTxToUtxoMap
  , releaseMempool
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.QueryM
  ( acquireMempoolSnapshot
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  ) as QueryM
import Ctl.Internal.QueryM.Ogmios
  ( MempoolReleased(Released)
  , MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , TxHash
  ) as Ogmios
import Ctl.Internal.TxOutput
  ( ogmiosTxOutToTransactionOutput
  , txOutRefToTransactionInput
  )
import Data.Array ((..), (:))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.UInt as UInt

-- | Establish a connection with the Local TX Monitor.
-- | Instantly accquires the current mempool snapshot, and will wait for the next
-- | mempool snapshot if used again before using `releaseMempool`.
acquireMempoolSnapshot :: Contract Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshot = wrapQueryM QueryM.acquireMempoolSnapshot

-- | Check to see if a TxHash is present in the current mempool snapshot.
mempoolSnapshotHasTx
  :: Ogmios.MempoolSnapshotAcquired -> Ogmios.TxHash -> Contract Boolean
mempoolSnapshotHasTx ms = wrapQueryM <<< QueryM.mempoolSnapshotHasTx ms

-- | Get the first received TX in the current mempool snapshot. This function can
-- | be recursively called to traverse the finger-tree of the mempool data set.
-- | This will return `Nothing` once it has reached the end of the current mempool.
mempoolSnapshotNextTx
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Maybe Ogmios.MempoolTransaction)
mempoolSnapshotNextTx = wrapQueryM <<< QueryM.mempoolSnapshotNextTx

-- | The acquired snapshot’s size (in bytes), number of transactions, and
-- | capacity (in bytes).
mempoolSnapshotSizeAndCapacity
  :: Ogmios.MempoolSnapshotAcquired -> Contract Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity = wrapQueryM <<<
  QueryM.mempoolSnapshotSizeAndCapacity

-- | Release the connection to the Local TX Monitor.
releaseMempool
  :: Ogmios.MempoolSnapshotAcquired -> Contract Ogmios.MempoolReleased
releaseMempool = wrapQueryM <<< QueryM.releaseMempool

-- | Recursively request the next TX in the mempool until Ogmios does not
-- | respond with a new TX.
fetchMempoolTXs
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Array Ogmios.MempoolTransaction)
fetchMempoolTXs ms = do
  nextTX <- mempoolSnapshotNextTx ms
  case nextTX of
    Just tx -> do
      txs <- fetchMempoolTXs ms
      pure $ tx : txs
    Nothing -> pure mempty

-- | Convert the UTxOs in the `MempoolTransaction` type into a `UtxoMap`.
mempoolTxToUtxoMap :: Ogmios.MempoolTransaction -> UtxoMap
mempoolTxToUtxoMap (Ogmios.MempoolTransaction { id, body }) =
  Map.fromFoldable $ Array.catMaybes $ uncurry tupleMaybes
    <$> bimap txOutRefToTransactionInput
      (toPlutusTxOutputWithRefScript <=< ogmiosTxOutToTransactionOutput)
    <$> Array.zipWith (\i x -> Tuple { txId: id, index: UInt.fromInt i } x)
      (0 .. (length outputs - 1))
      outputs

  where
  outputs = (unwrap body).outputs

  -- | Combine two `Maybe`s into a `Maybe Tuple`.
  tupleMaybes :: forall a b. Maybe a -> Maybe b -> Maybe (Tuple a b)
  tupleMaybes ma mb = (ma # map Tuple) <*> mb
