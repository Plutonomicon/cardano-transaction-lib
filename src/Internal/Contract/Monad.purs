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
  , runKupmiosM
  , wrapKupmiosM
  , stopContractEnv
  , withContractEnv
  , buildBackend
  , getLedgerConstants
  , filterLockedUtxos
  , getProvider
  , mkProvider
  ) where

import Prelude

import Cardano.Blockfrost.Service (BlockfrostServiceM, runBlockfrostServiceM)
import Cardano.Blockfrost.Service as Blockfrost
import Cardano.Kupmios (KupmiosConfig, KupmiosM, mkKupmiosEnv)
import Cardano.Kupmios.Ogmios (getProtocolParameters, getSystemStartTime)
import Cardano.Kupmios.Ogmios.Types (OgmiosDecodeError, pprintOgmiosDecodeError)
import Cardano.Provider.Error (ClientError)
import Cardano.Provider.Type (Provider)
import Cardano.Types (NetworkId(TestnetId, MainnetId), TransactionHash, UtxoMap)
import Cardano.Types.ProtocolParameters (ProtocolParameters)
import Cardano.Types.SystemStart (SystemStart)
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
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.Provider
  ( providerForBlockfrostBackend
  , providerForCtlBackend
  , providerForSelfHostedBlockfrostBackend
  )
import Ctl.Internal.Contract.ProviderBackend
  ( CtlBackend
  , CtlBackendParams
  , ProviderBackend(BlockfrostBackend, CtlBackend)
  , ProviderBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , getCtlBackend
  )
import Ctl.Internal.Helpers (filterMapWithKeyM, liftM, logWithLevel)
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed, newUsedTxOuts)
import Ctl.Internal.Wallet (Wallet(GenericCip30))
import Ctl.Internal.Wallet.Spec (WalletSpec, mkWalletBySpec)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds, Seconds)
import Data.Traversable (for_, traverse)
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
  { backend :: ProviderBackend
  , provider :: Provider
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

getProvider :: Contract Provider
getProvider = asks _.provider

mkProvider
  :: forall (rest :: Row Type). LogParams rest -> ProviderBackend -> Provider
mkProvider params providerBackend =
  case providerBackend of
    CtlBackend ctlBackend _ ->
      providerForCtlBackend runKupmiosM params ctlBackend
    BlockfrostBackend blockfrostBackend Nothing -> do
      providerForBlockfrostBackend params blockfrostBackend
    BlockfrostBackend blockfrostBackend (Just ctlBackend) -> do
      providerForSelfHostedBlockfrostBackend params blockfrostBackend
        runKupmiosM
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
        { backend, ledgerConstants, provider: mkProvider params backend }
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

buildBackend :: Logger -> ProviderBackendParams -> Aff ProviderBackend
buildBackend _ = case _ of
  CtlBackendParams ctlParams blockfrostParams ->
    flip CtlBackend blockfrostParams <$> buildCtlBackend ctlParams
  BlockfrostBackendParams blockfrostParams ctlParams ->
    BlockfrostBackend blockfrostParams <$> traverse buildCtlBackend ctlParams
  where
  buildCtlBackend :: CtlBackendParams -> Aff CtlBackend
  buildCtlBackend { ogmiosConfig, kupoConfig } = do
    pure
      { ogmiosConfig
      , kupoConfig
      }

-- | Query for the ledger constants using the main backend.
getLedgerConstants
  :: forall (r :: Row Type)
   . { logLevel :: LogLevel
     , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
     | r
     }
  -> ProviderBackend
  -> Aff LedgerConstants
getLedgerConstants params = case _ of
  CtlBackend ctlBackend _ -> do
    let
      logParams =
        { logLevel: params.logLevel
        , customLogger: params.customLogger
        , suppressLogs: true
        }
    pparams <- unwrap <$>
      ( runKupmiosM logParams ctlBackend getProtocolParameters >>=
          throwOnLeft
      )
    systemStart <- unwrap <$>
      ( runKupmiosM logParams ctlBackend getSystemStartTime >>=
          throwOnLeft
      )
    pure { pparams, systemStart }

    where
    throwOnLeft
      :: forall a
       . Either OgmiosDecodeError a
      -> Aff a
    throwOnLeft = case _ of
      Left err -> throwError $ error $ pprintOgmiosDecodeError err
      Right x -> pure x

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
stopContractEnv _ = pure unit

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
  { backendParams :: ProviderBackendParams
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
-- KupmiosM
--------------------------------------------------------------------------------

wrapKupmiosM :: forall (a :: Type). KupmiosM a -> Contract a
wrapKupmiosM qm = do
  backend <- asks _.backend
  ctlBackend <-
    getCtlBackend backend
      # liftM (error "Operation only supported on CTL backend")
  contractEnv <- ask
  liftAff $ runKupmiosM contractEnv ctlBackend qm

runKupmiosM
  :: forall (a :: Type) (rest :: Row Type)
   . LogParams rest
  -> CtlBackend
  -> KupmiosM a
  -> Aff a
runKupmiosM params ctlBackend action = do
  env <- mkKupmiosEnv config
  runReaderT (unwrap action) env
  where
  config :: KupmiosConfig
  config =
    { ogmios:
        { serverConfig: ctlBackend.ogmiosConfig
        , maxParallelRequests: Just 5
        }
    , kupo:
        { serverConfig: ctlBackend.kupoConfig
        }
    , logLevel: params.logLevel
    , customLogger: params.customLogger
    , suppressLogs: params.suppressLogs
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

