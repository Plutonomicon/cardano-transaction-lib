module Ctl.Internal.Contract.Monad
  ( Contract(Contract)
  , ParContract(ParContract)
  , ContractEnv
  , ContractParams
  , mkContractEnv
  , runContract
  , runContractInEnv
  , runQueryM
  , wrapQueryM
  , stopContractEnv
  , withContractEnv
  , buildBackend
  , getLedgerConstants
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel
  ( class Parallel
  , parTraverse
  , parTraverse_
  , parallel
  , sequential
  )
import Control.Plus (class Plus)
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.Contract.QueryBackend
  ( CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendLabel(CtlBackendLabel)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , QueryBackends
  , defaultBackend
  , lookupBackend
  )
import Ctl.Internal.Helpers (liftM, liftedM, logWithLevel)
import Ctl.Internal.JsWebSocket (_wsClose, _wsFinalize)
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
import Ctl.Internal.QueryM
  ( QueryEnv
  , QueryM
  , WebSocket
  , getEraSummariesAff
  , getProtocolParametersAff
  , getSystemStartAff
  , mkDatumCacheWebSocketAff
  , mkOgmiosWebSocketAff
  , underlyingWebSocket
  )
-- TOD Move/translate these types into Cardano
import Ctl.Internal.QueryM.Ogmios
  ( ProtocolParameters
  , RelativeTime
  , SlotLength
  , SystemStart
  ) as Ogmios
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Ctl.Internal.Serialization.Address (NetworkId, Slot)
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, newUsedTxOuts)
import Ctl.Internal.Wallet (Wallet)
import Ctl.Internal.Wallet (getNetworkId) as Wallet
import Ctl.Internal.Wallet.Spec (WalletSpec, mkWalletBySpec)
import Data.Either (Either(Left, Right))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, attempt, error, finally, supervise)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw, try)
import Effect.Ref (new) as Ref
import MedeaPrelude (class MonadAff)
import Record.Builder (build, merge)
import Undefined (undefined)

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

type ContractEnv =
  { backend :: QueryBackends QueryBackend
  -- ctlServer is currently used for applyArgs, which is needed for all backends. This will be removed later
  , ctlServerConfig :: Maybe ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  -- TODO: Duplicate types to Contract
  -- We don't support changing protocol parameters, so supporting a HFC is even less unlikely
  -- slotLength can only change with a HFC.
  , ledgerConstants ::
      { pparams :: Ogmios.ProtocolParameters
      , systemStart :: Ogmios.SystemStart
      -- Why not use Duration?
      , slotLength :: Ogmios.SlotLength
      -- A reference point in which the slotLength is assumed to be constant from
      -- then until now, not including HFC which occur during contract evaluation
      -- TODO: Drop systemStart and just normalize time AOT
      --       Maybe not drop it, we export it in Contract. I'm not sure why though
      -- TODO: We need to indicate that calculations in the past may be inaccurate
      --       Or enforce slot reference to be as 'relatively old'
      , slotReference :: { slot :: Slot, time :: Ogmios.RelativeTime }
      }
  }

-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: ContractParams
  -> Aff ContractEnv
mkContractEnv params = do
  for_ params.hooks.beforeInit (void <<< liftEffect <<< try)

  usedTxOuts <- newUsedTxOuts

  envBuilder <- sequential ado
    b1 <- parallel do
      backend <- buildBackend logger params.backendParams
      -- Use the default backend to fetch ledger constants
      ledgerConstants <- getLedgerConstants logger $ defaultBackend backend
      pure $ merge { backend, ledgerConstants }
    b2 <- parallel do
      wallet <- buildWallet
      pure $ merge { wallet }
    -- compose the sub-builders together
    in b1 >>> b2 >>> merge { usedTxOuts }

  pure $ build envBuilder constants
  where
  logger :: Logger
  logger = mkLogger params.logLevel params.customLogger

  buildWallet :: Aff (Maybe Wallet)
  buildWallet = traverse (mkWalletBySpec params.networkId) params.walletSpec

  constants =
    { ctlServerConfig: params.ctlServerConfig
    , networkId: params.networkId
    , logLevel: params.logLevel
    , walletSpec: params.walletSpec
    , customLogger: params.customLogger
    , suppressLogs: params.suppressLogs
    , hooks: params.hooks
    }

buildBackend
  :: Logger
  -> QueryBackends QueryBackendParams
  -> Aff (QueryBackends QueryBackend)
buildBackend logger = parTraverse case _ of
  CtlBackendParams { ogmiosConfig, kupoConfig, odcConfig } -> do
    datumCacheWsRef <- liftEffect $ Ref.new Nothing
    -- TODO Check the network in env matches up with the network of odc, ogmios and kupo
    --      Need to pass in the networkid
    sequential ado
      odcWs <- parallel $ mkDatumCacheWebSocketAff datumCacheWsRef logger
        odcConfig
      ogmiosWs <- parallel $ mkOgmiosWebSocketAff datumCacheWsRef logger
        ogmiosConfig
      in
        CtlBackend
          { ogmios:
              { config: ogmiosConfig
              , ws: ogmiosWs
              }
          , odc:
              { config: odcConfig
              , ws: odcWs
              }
          , kupoConfig
          }
  BlockfrostBackendParams bf -> pure $ BlockfrostBackend bf

getLedgerConstants
  :: Logger
  -> QueryBackend
  -> Aff
       { pparams :: Ogmios.ProtocolParameters
       , systemStart :: Ogmios.SystemStart
       , slotLength :: Ogmios.SlotLength
       , slotReference :: { slot :: Slot, time :: Ogmios.RelativeTime }
       }
getLedgerConstants logger = case _ of
  CtlBackend { ogmios: { ws } } -> do
    pparams <- getProtocolParametersAff ws logger
    systemStart <- getSystemStartAff ws logger
    -- Do we ever recieve an eraSummary ahead of schedule?
    -- Maybe search for the chainTip's era
    latestEraSummary <- liftedM (error "Could not get EraSummary") do
      map unwrap
        <<<
          (maximumBy (compare `on` (unwrap >>> _.start >>> unwrap >>> _.slot)))
        <<< unwrap <$> getEraSummariesAff ws logger
    let
      slotLength = _.slotLength $ unwrap $ _.parameters $ latestEraSummary
      slotReference = (\{ slot, time } -> { slot, time }) $ unwrap $ _.start $
        latestEraSummary
    pure { pparams, slotLength, systemStart, slotReference }
  BlockfrostBackend _ -> undefined

-- | Ensure that `NetworkId` from wallet is the same as specified in the
-- | `ContractEnv`.
walletNetworkCheck :: NetworkId -> Wallet -> Aff Unit
walletNetworkCheck envNetworkId wallet = do
  networkId <- Wallet.getNetworkId wallet
  unless (envNetworkId == networkId) do
    liftEffect $ throw $
      "The networkId that is specified is not equal to the one from wallet."
        <> " The wallet is using "
        <> show networkId
        <> " while "
        <> show envNetworkId
        <> " is specified in the config."

-- | Finalizes a `Contract` environment.
-- | Closes the connections in `ContractEnv`, effectively making it unusable.
stopContractEnv
  :: ContractEnv
  -> Aff Unit
stopContractEnv contractEnv = do
  flip parTraverse_ contractEnv.backend case _ of
    CtlBackend { ogmios, odc } -> do
      let
        stopWs :: forall (a :: Type). WebSocket a -> Effect Unit
        stopWs = ((*>) <$> _wsFinalize <*> _wsClose) <<< underlyingWebSocket
      liftEffect $ stopWs odc.ws
      liftEffect $ stopWs ogmios.ws
    BlockfrostBackend _ -> undefined

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

-- | Options to construct a `ContractEnv` indirectly.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can be run in parallel, reusing the same
-- | environment (see `withContractEnv`)
type ContractParams =
  { backendParams :: QueryBackends QueryBackendParams
  , ctlServerConfig :: Maybe ServerConfig
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

wrapQueryM :: forall (a :: Type). QueryM a -> Contract a
wrapQueryM qm = do
  backend <- asks _.backend
  ctlBackend <- liftM (error "Operation only supported on CTL backend") $
    lookupBackend CtlBackendLabel backend >>= case _ of
      CtlBackend b -> Just b
      _ -> Nothing
  contractEnv <- ask
  liftAff $ runQueryM contractEnv ctlBackend qm

runQueryM :: forall (a :: Type). ContractEnv -> CtlBackend -> QueryM a -> Aff a
runQueryM contractEnv ctlBackend =
  flip runReaderT (mkQueryEnv contractEnv ctlBackend) <<< unwrap

mkQueryEnv :: ContractEnv -> CtlBackend -> QueryEnv ()
mkQueryEnv contractEnv ctlBackend =
  { config:
      { ctlServerConfig: contractEnv.ctlServerConfig
      , datumCacheConfig: ctlBackend.odc.config
      , ogmiosConfig: ctlBackend.ogmios.config
      , kupoConfig: ctlBackend.kupoConfig
      , networkId: contractEnv.networkId
      , logLevel: contractEnv.logLevel
      , walletSpec: contractEnv.walletSpec
      , customLogger: contractEnv.customLogger
      , suppressLogs: contractEnv.suppressLogs
      }
  , runtime:
      { ogmiosWs: ctlBackend.ogmios.ws
      , datumCacheWs: ctlBackend.odc.ws
      , wallet: contractEnv.wallet
      , usedTxOuts: contractEnv.usedTxOuts
      , pparams: contractEnv.ledgerConstants.pparams
      }
  , extraConfig: {}
  }

