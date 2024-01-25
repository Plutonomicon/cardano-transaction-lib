module Ctl.Internal.Contract.Monad
  ( Contract(Contract)
  , ContractEnv
  , ContractParams
  , ContractTimeParams
  , ContractSynchronizationParams
  , LedgerConstants
  , ParContract(ParContract)
  , mkContractEnv
  , runContract
  , runContractInEnv
  , runQueryM
  , wrapQueryM
  , stopContractEnv
  , withContractEnv
  , buildBackend
  , getLedgerConstants
  , filterLockedUtxos
  , getQueryHandle
  , mkQueryHandle
  ) where

import Prelude

import Contract.Prelude (liftEither)
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.QueryBackend
  ( CtlBackend
  , CtlBackendParams
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , getCtlBackend
  )
import Ctl.Internal.Contract.QueryHandle
  ( queryHandleForBlockfrostBackend
  , queryHandleForCtlBackend
  , queryHandleForSelfHostedBlockfrostBackend
  )
import Ctl.Internal.Contract.QueryHandle.Type (QueryHandle)
import Ctl.Internal.Helpers (filterMapWithKeyM, liftM, logWithLevel)
import Ctl.Internal.JsWebSocket (_wsClose, _wsFinalize)
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
import Ctl.Internal.QueryM
  ( QueryEnv
  , QueryM
  , WebSocket
  , getProtocolParametersAff
  , getSystemStartAff
  , mkOgmiosWebSocketAff
  , underlyingWebSocket
  )
import Ctl.Internal.QueryM.Kupo (isTxConfirmedAff)
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId, MainnetId))
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.ProtocolParameters (ProtocolParameters)
import Ctl.Internal.Types.SystemStart (SystemStart)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed, newUsedTxOuts)
import Ctl.Internal.Wallet (Wallet(GenericCip30))
import Ctl.Internal.Wallet.Spec (WalletSpec, mkWalletBySpec)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), isRight)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds, Seconds)
import Data.Traversable (for_, traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, attempt, error, finally, supervise)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw, try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Record.Builder (build, merge)

--------------------------------------------------------------------------------
-- Contract
--------------------------------------------------------------------------------

-- | The `Contract` monad is a newtype wrapper over `ReaderT` on `ContractEnv`
-- | over asynchronous effects, `Aff`. Throwing and catching errors can
-- | therefore be implemented with native JavaScript `Effect.Exception.Error`s
-- | and `Effect.Class.Console.log` replaces the `Writer` monad. `Aff` enables
-- | the user to make effectful calls inside this `Contract` monad.
newtype Contract (a :: Type) = Contract (ReaderT ContractEnv Aff a)

-- Many of these derivations depend on the underlying `ReaderT` and
-- asychronous effects, `Aff`.
derive instance Newtype (Contract a) _
derive newtype instance Functor Contract
derive newtype instance Apply Contract
derive newtype instance Applicative Contract
derive newtype instance Alt Contract
derive newtype instance Plus Contract
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

instance Parallel ParContract Contract where
  parallel :: Contract ~> ParContract
  parallel (Contract a) = ParContract $ parallel a
  sequential :: ParContract ~> Contract
  sequential (ParContract a) = Contract $ sequential a

newtype ParContract (a :: Type) = ParContract
  (ReaderT ContractEnv ParAff a)

derive newtype instance Functor ParContract
derive newtype instance Apply ParContract
derive newtype instance Applicative ParContract
derive newtype instance Alt ParContract
derive newtype instance Plus ParContract
derive newtype instance Alternative ParContract
derive newtype instance Semigroup a => Semigroup (ParContract a)
derive newtype instance Monoid a => Monoid (ParContract a)

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

-- | `LedgerConstants` contains values that technically may change, but we assume
-- | to be constant during Contract evaluation.
type LedgerConstants =
  { pparams :: ProtocolParameters
  , systemStart :: SystemStart
  }

-- | A record containing `Contract` environment - everything a `Contract` needs
-- | to run. It is recommended to use one environment per application to save
-- | on websocket connections and to keep track of `UsedTxOuts`.
type ContractEnv =
  { backend :: QueryBackend
  , handle :: QueryHandle
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  , ledgerConstants :: LedgerConstants
  , timeParams :: ContractTimeParams
  , synchronizationParams :: ContractSynchronizationParams
  , knownTxs ::
      { backend :: Ref (Set TransactionHash)
      }
  }

getQueryHandle :: Contract QueryHandle
getQueryHandle = asks _.handle

mkQueryHandle
  :: forall (rest :: Row Type). LogParams rest -> QueryBackend -> QueryHandle
mkQueryHandle params queryBackend =
  case queryBackend of
    CtlBackend ctlBackend _ ->
      queryHandleForCtlBackend runQueryM params ctlBackend
    BlockfrostBackend blockfrostBackend Nothing -> do
      queryHandleForBlockfrostBackend params blockfrostBackend
    BlockfrostBackend blockfrostBackend (Just ctlBackend) -> do
      queryHandleForSelfHostedBlockfrostBackend params blockfrostBackend
        runQueryM
        ctlBackend

-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: ContractParams
  -> Aff ContractEnv
mkContractEnv params = do
  for_ params.hooks.beforeInit (void <<< liftEffect <<< try)

  usedTxOuts <- newUsedTxOuts
  backend <- liftEffect $ Ref.new Set.empty

  envBuilder <- sequential ado
    b1 <- parallel do
      backend <- buildBackend logger params.backendParams
      ledgerConstants <- getLedgerConstants params backend
      pure $ merge
        { backend, ledgerConstants, handle: mkQueryHandle params backend }
    b2 <- parallel do
      wallet <- buildWallet
      pure $ merge { wallet }
    -- Compose the sub-builders together
    in
      b1 >>> b2 >>> merge
        { usedTxOuts
        , timeParams: params.timeParams
        , synchronizationParams: params.synchronizationParams
        , knownTxs: { backend }
        }
  pure $ build envBuilder constants
  where
  logger :: Logger
  logger = mkLogger params.logLevel params.customLogger

  buildWallet :: Aff (Maybe Wallet)
  buildWallet = traverse mkWalletBySpec params.walletSpec

  constants =
    { networkId: params.networkId
    , logLevel: params.logLevel
    , customLogger: params.customLogger
    , suppressLogs: params.suppressLogs
    , hooks: params.hooks
    }

buildBackend :: Logger -> QueryBackendParams -> Aff QueryBackend
buildBackend logger = case _ of
  CtlBackendParams ctlParams blockfrostParams ->
    flip CtlBackend blockfrostParams <$> buildCtlBackend ctlParams
  BlockfrostBackendParams blockfrostParams ctlParams ->
    BlockfrostBackend blockfrostParams <$> traverse buildCtlBackend ctlParams
  where
  buildCtlBackend :: CtlBackendParams -> Aff CtlBackend
  buildCtlBackend { ogmiosConfig, kupoConfig } = do
    let isTxConfirmed = map isRight <<< isTxConfirmedAff kupoConfig <<< wrap
    ogmiosWs <- mkOgmiosWebSocketAff isTxConfirmed logger ogmiosConfig
    pure
      { ogmios:
          { config: ogmiosConfig
          , ws: ogmiosWs
          }
      , kupoConfig
      }

-- | Query for the ledger constants using the main backend.
getLedgerConstants
  :: forall (r :: Row Type)
   . { logLevel :: LogLevel
     , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
     | r
     }
  -> QueryBackend
  -> Aff LedgerConstants
getLedgerConstants params = case _ of
  CtlBackend { ogmios: { ws } } _ ->
    { pparams: _, systemStart: _ }
      <$> (unwrap <$> getProtocolParametersAff ws logger)
      <*> getSystemStartAff ws logger
  BlockfrostBackend backend _ ->
    runBlockfrostServiceM blockfrostLogger backend $
      { pparams: _, systemStart: _ }
        <$> withErrorOnLeft Blockfrost.getProtocolParameters
        <*> withErrorOnLeft Blockfrost.getSystemStart
  where
  withErrorOnLeft
    :: forall (a :: Type)
     . BlockfrostServiceM (Either ClientError a)
    -> BlockfrostServiceM a
  withErrorOnLeft = (=<<) (lmap (show >>> error) >>> liftEither)

  logger :: Logger
  logger = mkLogger params.logLevel params.customLogger

  -- TODO: Should we respect `suppressLogs` here?
  blockfrostLogger :: Message -> Aff Unit
  blockfrostLogger = fromMaybe logWithLevel params.customLogger params.logLevel

-- | Ensure that `NetworkId` from wallet is the same as specified in the
-- | `ContractEnv`.
-- todo: reimplement uniformly
walletNetworkCheck :: NetworkId -> Wallet -> Aff Unit
walletNetworkCheck envNetworkId =
  case _ of
    GenericCip30 wallet -> do
      check =<< intToNetworkId =<< wallet.getNetworkId
    _ -> pure unit
  where
  check :: NetworkId -> Aff Unit
  check networkId = unless (envNetworkId == networkId) do
    liftEffect $ throw $
      "The networkId that is specified is not equal to the one from wallet."
        <> " The wallet is using "
        <> show networkId
        <> " while "
        <> show envNetworkId
        <> " is specified in the config."

  intToNetworkId :: Int -> Aff NetworkId
  intToNetworkId = case _ of
    0 -> pure TestnetId
    1 -> pure MainnetId
    _ -> liftEffect $ throw "Unknown network id"

-- | Finalizes a `Contract` environment.
-- | Closes the connections in `ContractEnv`, effectively making it unusable.
stopContractEnv :: ContractEnv -> Aff Unit
stopContractEnv { backend } =
  liftEffect $ traverse_ stopCtlRuntime (getCtlBackend backend)
  where
  stopCtlRuntime :: CtlBackend -> Effect Unit
  stopCtlRuntime { ogmios } =
    stopWebSocket ogmios.ws

  stopWebSocket :: forall (a :: Type). WebSocket a -> Effect Unit
  stopWebSocket = ((*>) <$> _wsFinalize <*> _wsClose) <<< underlyingWebSocket

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

  contractEnv <- mkContractEnv params { customLogger = customLogger }
  for_ contractEnv.wallet $ walletNetworkCheck contractEnv.networkId
  eiRes <-
    attempt $ supervise (action contractEnv)
      `flip finally` stopContractEnv contractEnv
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

-- | Delays and timeouts for internal query functions.
-- |
-- | - `awaitTxConfirmed.delay` - how frequently should we query for Tx in
-- | `Contract.Transaction.awaitTxConfirmed`
-- |
-- | - For info on `syncBackend` and syncWallet` see `doc/query-layers.md`
type ContractTimeParams =
  { awaitTxConfirmed :: { delay :: Milliseconds, timeout :: Seconds }
  , waitUntilSlot :: { delay :: Milliseconds }
  , syncWallet :: { delay :: Milliseconds, timeout :: Seconds }
  , syncBackend :: { delay :: Milliseconds, timeout :: Seconds }
  }

type ContractSynchronizationParams =
  { syncBackendWithWallet ::
      { errorOnTimeout :: Boolean
      , beforeCip30Methods :: Boolean
      , beforeBalancing :: Boolean
      }
  , syncWalletWithTxInputs ::
      { errorOnTimeout :: Boolean, beforeCip30Sign :: Boolean }
  , syncWalletWithTransaction ::
      { errorOnTimeout :: Boolean, beforeTxConfirmed :: Boolean }
  }

-- | Options to construct an environment for a `Contract` to run.
-- |
-- | See `Contract.Config` for pre-defined values for testnet and mainnet.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can reuse the same environment.
type ContractParams =
  { backendParams :: QueryBackendParams
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  -- | Suppress logs until an exception is thrown
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , timeParams :: ContractTimeParams
  , synchronizationParams :: ContractSynchronizationParams
  }

--------------------------------------------------------------------------------
-- QueryM
--------------------------------------------------------------------------------

wrapQueryM :: forall (a :: Type). QueryM a -> Contract a
wrapQueryM qm = do
  backend <- asks _.backend
  ctlBackend <-
    getCtlBackend backend
      # liftM (error "Operation only supported on CTL backend")
  contractEnv <- ask
  liftAff $ runQueryM contractEnv ctlBackend qm

runQueryM
  :: forall (a :: Type) (rest :: Row Type)
   . LogParams rest
  -> CtlBackend
  -> QueryM a
  -> Aff a
runQueryM params ctlBackend =
  flip runReaderT (mkQueryEnv params ctlBackend) <<< unwrap

mkQueryEnv
  :: forall (rest :: Row Type). LogParams rest -> CtlBackend -> QueryEnv
mkQueryEnv params ctlBackend =
  { config:
      { kupoConfig: ctlBackend.kupoConfig
      , logLevel: params.logLevel
      , customLogger: params.customLogger
      , suppressLogs: params.suppressLogs
      }
  , runtime:
      { ogmiosWs: ctlBackend.ogmios.ws
      }
  }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

filterLockedUtxos :: UtxoMap -> Contract UtxoMap
filterLockedUtxos utxos =
  withTxRefsCache $
    flip filterMapWithKeyM utxos
      (\k _ -> not <$> isTxOutRefUsed (unwrap k))

withTxRefsCache :: forall (a :: Type). ReaderT UsedTxOuts Aff a -> Contract a
withTxRefsCache = Contract <<< withReaderT _.usedTxOuts
