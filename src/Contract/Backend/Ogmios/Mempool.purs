-- | A module for interacting with Ogmios' Local TX Monitor
-- | https://ogmios.dev/mini-protocols/local-tx-monitor/
module Contract.Backend.Ogmios.Mempool
  ( module Ogmios
  , acquireMempoolSnapshot
  , fetchMempoolTxs
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , mempoolTxToUtxoMap
  , releaseMempool
  , withMempoolSnapshot
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Utxos (UtxoMap)
import Control.Apply (lift2)
import Control.Monad.Error.Class (try)
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
import Data.Array (range)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.List (List(Cons))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.UInt as UInt
import Effect.Exception (throw)

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
  :: Ogmios.MempoolSnapshotAcquired -> Contract Unit
releaseMempool = void <<< wrapQueryM <<< QueryM.releaseMempool

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
  -> Contract (Array Ogmios.MempoolTransaction)
fetchMempoolTxs ms = Array.fromFoldable <$> go
  where
  go = do
    nextTX <- mempoolSnapshotNextTx ms
    case nextTX of
      Just tx -> Cons tx <$> go
      Nothing -> pure mempty

-- | Convert the UTxOs in the `MempoolTransaction` type into a `UtxoMap`.
mempoolTxToUtxoMap :: Ogmios.MempoolTransaction -> Contract UtxoMap
mempoolTxToUtxoMap tx@(Ogmios.MempoolTransaction { id, body }) = do
  let
    tuples = uncurry (lift2 Tuple)
      <$> bimap txOutRefToTransactionInput
        (toPlutusTxOutputWithRefScript <=< ogmiosTxOutToTransactionOutput)
      <$> Array.zipWith (\i x -> Tuple { txId: id, index: UInt.fromInt i } x)
        (range 0 (length outputs - 1))
        outputs
  Map.fromFoldable <$> for tuples
    ( maybe
        ( liftEffect $ throw
            $ "mempoolTxToUtxoMap: impossible happened (conversion"
            <> " failure), please report as bug: "
            <> show tx
        )
        pure
    )
  where
  outputs = (unwrap body).outputs
