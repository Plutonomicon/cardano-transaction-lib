-- | A module for interacting with Ogmios' Local TX Monitor
-- | These functions only work with Ogmios backend (not Blockfrost!).
-- | https://ogmios.dev/mini-protocols/local-tx-monitor/
module Contract.Backend.Ogmios.Mempool
  ( acquireMempoolSnapshot
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , fetchMempoolTxs
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  , withMempoolSnapshot
  , MempoolEnv
  , MempoolMT(MempoolMT)
  , MempoolM
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types.Transaction (Transaction)
import Cardano.Types.TransactionHash (TransactionHash)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , liftMaybe
  , try
  )
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), asks)
import Ctl.Internal.Logging (Logger, mkLogger)
import Ctl.Internal.QueryM.Ogmios.Mempool
  ( ListenerSet
  , OgmiosListeners
  , OgmiosWebSocket
  , acquireMempoolSnapshotCall
  , listeners
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , mkRequestAff
  , releaseMempoolCall
  , underlyingWebSocket
  )
import Ctl.Internal.QueryM.Ogmios.Mempool
  ( MempoolSizeAndCapacity
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  ) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Mempool.JsWebSocket (JsWebSocket)
import Ctl.Internal.QueryM.Ogmios.Mempool.JsonRpc2 as JsonRpc2
import Data.Array as Array
import Data.ByteArray (hexToByteArray)
import Data.List (List(Cons))
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)

----------------
-- Mempool monad
----------------

type MempoolEnv =
  { ogmiosWs :: OgmiosWebSocket
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  }

type MempoolM = MempoolMT Aff

newtype MempoolMT (m :: Type -> Type) (a :: Type) =
  MempoolMT (ReaderT MempoolEnv m a)

derive instance Newtype (MempoolMT m a) _
derive newtype instance Functor m => Functor (MempoolMT m)
derive newtype instance Apply m => Apply (MempoolMT m)
derive newtype instance Applicative m => Applicative (MempoolMT m)
derive newtype instance Bind m => Bind (MempoolMT m)
derive newtype instance Monad (MempoolMT Aff)
derive newtype instance MonadEffect (MempoolMT Aff)
derive newtype instance MonadAff (MempoolMT Aff)
derive newtype instance MonadThrow Error (MempoolMT Aff)
derive newtype instance MonadError Error (MempoolMT Aff)
derive newtype instance MonadAsk MempoolEnv (MempoolMT Aff)

--------------------
-- Mempool functions
--------------------

-- | A bracket-style function for working with mempool snapshots - ensures
-- | release in the presence of exceptions
withMempoolSnapshot
  :: forall a
   . (Ogmios.MempoolSnapshotAcquired -> MempoolM a)
  -> MempoolM a
withMempoolSnapshot f = do
  s <- acquireMempoolSnapshot
  res <- try $ f s
  releaseMempool s
  liftEither res

-- | Recursively request the next TX in the mempool until Ogmios does not
-- | respond with a new TX.
fetchMempoolTxs
  :: Ogmios.MempoolSnapshotAcquired
  -> MempoolM (Array Transaction)
fetchMempoolTxs ms = Array.fromFoldable <$> go
  where
  go = do
    nextTX <- mempoolSnapshotNextTx ms
    case nextTX of
      Just tx -> Cons tx <$> go
      Nothing -> pure mempty

acquireMempoolSnapshot
  :: MempoolM Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshot =
  mkOgmiosRequest
    acquireMempoolSnapshotCall
    _.acquireMempool
    unit

mempoolSnapshotHasTx
  :: Ogmios.MempoolSnapshotAcquired
  -> TransactionHash
  -> MempoolM Boolean
mempoolSnapshotHasTx ms txh =
  unwrap <$> mkOgmiosRequest
    (mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacity
  :: Ogmios.MempoolSnapshotAcquired
  -> MempoolM Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity ms =
  mkOgmiosRequest
    (mempoolSnapshotSizeAndCapacityCall ms)
    _.mempoolSizeAndCapacity
    unit

releaseMempool
  :: Ogmios.MempoolSnapshotAcquired
  -> MempoolM Unit
releaseMempool ms =
  unit <$ mkOgmiosRequest
    (releaseMempoolCall ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTx
  :: Ogmios.MempoolSnapshotAcquired
  -> MempoolM (Maybe Transaction)
mempoolSnapshotNextTx ms = do
  mbTx <- unwrap <$> mkOgmiosRequest
    (mempoolSnapshotNextTxCall ms)
    _.mempoolNextTx
    unit
  for mbTx \(Ogmios.MempoolTransaction { raw }) -> do
    byteArray <- liftMaybe (error "Failed to decode transaction")
      $ hexToByteArray raw
    liftMaybe (error "Failed to decode tx")
      $ decodeCbor
      $ wrap byteArray

-- | Builds an Ogmios request action using `MempoolM`
mkOgmiosRequest
  :: forall (request :: Type) (response :: Type)
   . JsonRpc2.JsonRpc2Call request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> MempoolM response
mkOgmiosRequest jsonRpc2Call getLs inp = do
  listeners' <- asks $ listeners <<< _.ogmiosWs
  websocket <- asks $ underlyingWebSocket <<< _.ogmiosWs
  mkRequest listeners' websocket jsonRpc2Call getLs inp

mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> JsonRpc2.JsonRpc2Call request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> MempoolM response
mkRequest listeners' ws jsonRpc2Call getLs inp = do
  logger <- getLogger
  liftAff $ mkRequestAff listeners' ws logger jsonRpc2Call getLs inp
  where
  getLogger :: MempoolM Logger
  getLogger = do
    logLevel <- asks $ _.logLevel
    mbCustomLogger <- asks $ _.customLogger
    pure $ mkLogger logLevel mbCustomLogger

