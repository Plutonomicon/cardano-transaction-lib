module Ctl.Internal.Contract.Monad
  ( Contract(Contract)
  , ContractEnv
  , ContractParams
  , mkContractEnv
  , runContract
  , runContractInEnv
  , runQueryM
  , stopContractEnv
  , withContractEnv
  ) where

import Prelude

import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (parallel, sequential)
import Ctl.Internal.Contract.QueryBackend
  ( CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendLabel(CtlBackendLabel)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , QueryBackends
  , lookupBackend
  )
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.JsWebSocket (JsWebSocket, _wsClose, _wsFinalize)
import Ctl.Internal.QueryM
  ( DatumCacheWebSocket
  , Hooks
  , Logger
  , OgmiosWebSocket
  , QueryEnv
  , QueryM
  , mkDatumCacheWebSocketAff
  , mkLogger
  , mkOgmiosWebSocketAff
  , mkWalletBySpec
  , underlyingWebSocket
  )
import Ctl.Internal.QueryM.Logging (setupLogs)
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters) as Ogmios
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, newUsedTxOuts)
import Ctl.Internal.Wallet (Wallet)
import Ctl.Internal.Wallet.Spec (WalletSpec)
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, finally, supervise)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, try)
import Effect.Ref (new) as Ref
import MedeaPrelude (class MonadAff)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- Contract
--------------------------------------------------------------------------------

newtype Contract (a :: Type) = Contract (ReaderT ContractEnv Aff a)

-- Many of these derivations depend on the underlying `ReaderT` and
-- asychronous effects, `Aff`.
derive instance Newtype (Contract a) _
derive newtype instance Functor Contract
derive newtype instance Apply Contract
derive newtype instance Applicative Contract
derive newtype instance Bind Contract
derive newtype instance Monad Contract
derive newtype instance MonadEffect Contract
derive newtype instance MonadAff Contract
derive newtype instance Semigroup a => Semigroup (Contract a)
derive newtype instance Monoid a => Monoid (Contract a)
derive newtype instance MonadRec Contract
derive newtype instance MonadAsk ContractEnv Contract
derive newtype instance MonadReader ContractEnv Contract
-- Utilise JavaScript's native `Error` via underlying `Aff` for flexibility:
derive newtype instance MonadThrow Error Contract
derive newtype instance MonadError Error Contract

instance MonadLogger Contract where
  log msg = do
    config <- ask
    let logFunction = fromMaybe logWithLevel config.customLogger
    liftAff $ logFunction config.logLevel msg

-- | Interprets a contract into an `Aff` context.
-- | Implicitly initializes and finalizes a new `ContractEnv` runtime.
-- |
-- | Use `withContractEnv` if your application contains multiple contracts that
-- | can be run in parallel, reusing the same environment (see
-- | `withContractEnv`)
runContract :: forall (a :: Type). ContractParams -> Contract a -> Aff a
runContract params contract = do
  withContractEnv params \config ->
    runContractInEnv config contract

-- | Runs a contract in existing environment. Does not destroy the environment
-- | when contract execution ends.
runContractInEnv :: forall (a :: Type). ContractEnv -> Contract a -> Aff a
runContractInEnv contractEnv =
  flip runReaderT contractEnv <<< unwrap

--------------------------------------------------------------------------------
-- ContractEnv
--------------------------------------------------------------------------------

type ContractEnv =
  { backend :: QueryBackends QueryBackend
  , ctlServerConfig :: Maybe ServerConfig
  , datumCache :: { config :: ServerConfig, ws :: DatumCacheWebSocket } -- TODO:
  -- , datumCache :: Maybe { config :: ServerConfig, ws :: DatumCacheWebSocket }
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  , pparams :: Ogmios.ProtocolParameters -- TODO:
  }

-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: Warn
       ( Text
           "Using `mkContractEnv` is not recommended: it does not ensure `ContractEnv` finalization. Consider using `withContractEnv`"
       )
  => ContractParams
  -> Aff ContractEnv
mkContractEnv params = do
  runtime <- mkContractRuntime params
  pure
    { backend: mkQueryBackend runtime <$> params.backendParams
    , ctlServerConfig: params.ctlServerConfig
    , datumCache: { config: params.datumCacheConfig, ws: runtime.datumCacheWs }
    , networkId: params.networkId
    , logLevel: params.logLevel
    , walletSpec: params.walletSpec
    , customLogger: params.customLogger
    , suppressLogs: params.suppressLogs
    , hooks: params.hooks
    , wallet: runtime.wallet
    , usedTxOuts: runtime.usedTxOuts
    , pparams: runtime.pparams
    }
  where
  mkQueryBackend :: ContractRuntime -> QueryBackendParams -> QueryBackend
  mkQueryBackend _ (BlockfrostBackendParams backend) =
    BlockfrostBackend backend
  mkQueryBackend runtime (CtlBackendParams { ogmiosConfig, kupoConfig }) =
    CtlBackend
      { ogmiosConfig
      , ogmiosWs: unsafePartial fromJust runtime.ogmiosWs
      , kupoConfig
      }

--------------------------------------------------------------------------------
-- ContractRuntime
--------------------------------------------------------------------------------

type ContractRuntime =
  { ogmiosWs :: Maybe OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  , pparams :: Ogmios.ProtocolParameters -- TODO:
  }

-- | Used in `mkContractRuntime` only.
data ContractRuntimeModel = ContractRuntimeModel
  DatumCacheWebSocket
  (Maybe OgmiosWebSocket)
  (Maybe Wallet)

mkContractRuntime :: ContractParams -> Aff ContractRuntime
mkContractRuntime params = do
  for_ params.hooks.beforeInit (void <<< liftEffect <<< try)
  usedTxOuts <- newUsedTxOuts
  datumCacheWsRef <- liftEffect $ Ref.new Nothing
  ContractRuntimeModel datumCacheWs ogmiosWs wallet <- sequential $
    ContractRuntimeModel
      <$> parallel
        ( mkDatumCacheWebSocketAff datumCacheWsRef logger
            params.datumCacheConfig
        )
      <*> parallel
        ( traverse (mkOgmiosWebSocketAff datumCacheWsRef logger)
            mOgmiosConfig
        )
      <*> parallel (traverse mkWalletBySpec params.walletSpec)
  pparams <- undefined -- TODO:
  pure { ogmiosWs, datumCacheWs, wallet, usedTxOuts, pparams }
  where
  logger :: Logger
  logger = mkLogger params.logLevel params.customLogger

  mOgmiosConfig :: Maybe ServerConfig
  mOgmiosConfig =
    lookupBackend CtlBackendLabel params.backendParams >>= case _ of
      CtlBackendParams { ogmiosConfig } -> Just ogmiosConfig
      _ -> Nothing

-- | Finalizes a `Contract` environment.
-- | Closes the websockets in `ContractEnv`, effectively making it unusable.
stopContractEnv
  :: Warn
       ( Text
           "Using `stopContractEnv` is not recommended: users should rely on `withContractEnv` to finalize the runtime environment instead"
       )
  => ContractEnv
  -> Effect Unit
stopContractEnv contractEnv = do
  _wsFinalize datumCacheWs *> _wsClose datumCacheWs
  for_ mOgmiosWs \ogmiosWs -> _wsFinalize ogmiosWs *> _wsClose ogmiosWs
  where
  datumCacheWs :: JsWebSocket
  datumCacheWs = underlyingWebSocket contractEnv.datumCache.ws

  mOgmiosWs :: Maybe JsWebSocket
  mOgmiosWs =
    lookupBackend CtlBackendLabel contractEnv.backend >>= case _ of
      CtlBackend { ogmiosWs } ->
        Just $ underlyingWebSocket ogmiosWs
      _ -> Nothing

-- | Constructs and finalizes a contract environment that is usable inside a
-- | bracket callback.
-- | One environment can be used by multiple `Contract`s in parallel (see
-- | `runContractInEnv`).
-- | Make sure that `Aff` action does not end before all contracts that use the
-- | runtime terminate. Otherwise `WebSocket`s will be closed too early.
withContractEnv
  :: forall (a :: Type). ContractParams -> (ContractEnv -> Aff a) -> Aff a
withContractEnv params action = do
  { addLogEntry, printLogs } <-
    liftEffect $ setupLogs params.logLevel params.customLogger
  let
    customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
    customLogger
      | params.suppressLogs = Just $ map liftEffect <<< addLogEntry
      | otherwise = params.customLogger

  contractEnv <- mkContractEnv params <#> _ { customLogger = customLogger }
  eiRes <-
    -- TODO: Adapt `networkIdCheck` from QueryM module
    attempt $ supervise (action contractEnv)
      `flip finally` liftEffect (stopContractEnv contractEnv)
  liftEffect $ case eiRes of
    Left err -> do
      for_ contractEnv.hooks.onError \f -> void $ try $ f err
      when contractEnv.suppressLogs printLogs
      throwError err
    Right res -> do
      for_ contractEnv.hooks.onSuccess (void <<< try)
      pure res

--------------------------------------------------------------------------------
-- ContractParams
--------------------------------------------------------------------------------

-- | Options to construct a `ContractEnv` indirectly.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can be run in parallel, reusing the same
-- | environment (see `withContractEnv`)
type ContractParams =
  { backendParams :: QueryBackends QueryBackendParams
  , ctlServerConfig :: Maybe ServerConfig
  , datumCacheConfig :: ServerConfig -- TODO:
  -- , datumCacheConfig :: Maybe ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  -- | Suppress logs until an exception is thrown
  , suppressLogs :: Boolean
  , hooks :: Hooks
  }

--------------------------------------------------------------------------------
-- QueryM
--------------------------------------------------------------------------------

runQueryM :: forall (a :: Type). ContractEnv -> CtlBackend -> QueryM a -> Aff a
runQueryM contractEnv ctlBackend =
  flip runReaderT (mkQueryEnv contractEnv ctlBackend) <<< unwrap

mkQueryEnv :: ContractEnv -> CtlBackend -> QueryEnv ()
mkQueryEnv contractEnv ctlBackend =
  { config:
      { ctlServerConfig: contractEnv.ctlServerConfig
      , datumCacheConfig: contractEnv.datumCache.config
      , ogmiosConfig: ctlBackend.ogmiosConfig
      , kupoConfig: ctlBackend.kupoConfig
      , networkId: contractEnv.networkId
      , logLevel: contractEnv.logLevel
      , walletSpec: contractEnv.walletSpec
      , customLogger: contractEnv.customLogger
      , suppressLogs: contractEnv.suppressLogs
      , hooks: contractEnv.hooks
      }
  , runtime:
      { ogmiosWs: ctlBackend.ogmiosWs
      , datumCacheWs: contractEnv.datumCache.ws
      , wallet: contractEnv.wallet
      , usedTxOuts: contractEnv.usedTxOuts
      , pparams: contractEnv.pparams
      }
  , extraConfig: {}
  }

