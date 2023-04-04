-- | Module for backend-specific functions that only work with Ogmios/Kupo backends
module Contract.Backend.Ogmios
  ( acquireMempoolSnapshot
  , fetchMempoolTXs
  , getPoolParameters
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , mempoolTxToUtxoMap
  , releaseMempool
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (PoolPubKeyHash)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Cardano.Types.Transaction (PoolRegistrationParams)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.QueryM
  ( acquireMempoolSnapshot
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  ) as QM
import Ctl.Internal.QueryM.Ogmios
  ( MempoolReleased
  , MempoolSizeAndCapacity
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , TxHash
  )
import Ctl.Internal.QueryM.Pools as QueryM
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

-- | **This function can only run with Ogmios backend**
-- |
-- | Blockfrost does not support fetching the required data:
-- | https://github.com/blockfrost/blockfrost-backend-ryo/issues/82
getPoolParameters
  :: PoolPubKeyHash
  -> Contract PoolRegistrationParams
getPoolParameters = wrapQueryM <<< QueryM.getPoolParameters

--------------------------------------------------------------------------------
-- Local Tx Monitor
--------------------------------------------------------------------------------

-- | Establish a connection with the Local TX Monitor.
-- | Instantly accquires the current mempool snapshot, and will wait for the next
-- | mempool snapshot if used again before using `releaseMempool`.
acquireMempoolSnapshot :: Contract MempoolSnapshotAcquired
acquireMempoolSnapshot = wrapQueryM QM.acquireMempoolSnapshot

-- | Check to see if a TxHash is present in the current mempool snapshot.
mempoolSnapshotHasTx :: MempoolSnapshotAcquired -> TxHash -> Contract Boolean
mempoolSnapshotHasTx ms = wrapQueryM <<< QM.mempoolSnapshotHasTx ms

-- | Get the first received TX in the current mempool snapshot. This function can
-- | be recursively called to traverse the finger-tree of the mempool data set.
-- | This will return `Nothing` once it has reached the end of the current mempool.
mempoolSnapshotNextTx
  :: MempoolSnapshotAcquired -> Contract (Maybe MempoolTransaction)
mempoolSnapshotNextTx = wrapQueryM <<< QM.mempoolSnapshotNextTx

-- | The acquired snapshot’s size (in bytes), number of transactions, and
-- | capacity (in bytes).
mempoolSnapshotSizeAndCapacity
  :: MempoolSnapshotAcquired -> Contract MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity = wrapQueryM <<<
  QM.mempoolSnapshotSizeAndCapacity

-- | Release the connection to the Local TX Monitor.
releaseMempool :: MempoolSnapshotAcquired -> Contract MempoolReleased
releaseMempool = wrapQueryM <<< QM.releaseMempool

-- | Recursively request the next TX in the mempool until Ogmios does not
-- | respond with a new TX.
fetchMempoolTXs
  :: MempoolSnapshotAcquired -> Contract (Array MempoolTransaction)
fetchMempoolTXs ms = do
  nextTX <- mempoolSnapshotNextTx ms
  case nextTX of
    Just tx -> do
      txs <- fetchMempoolTXs ms
      pure $ tx : txs
    Nothing -> pure mempty

-- | Convert the UTxOs in the `MempoolTransaction` type into a `UtxoMap`.
mempoolTxToUtxoMap :: MempoolTransaction -> UtxoMap
mempoolTxToUtxoMap (MempoolTransaction { id, body }) =
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
