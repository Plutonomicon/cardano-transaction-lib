module Ctl.Internal.Contract.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM (DatumCacheWebSocket, OgmiosWebSocket, Hooks)
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters) as Ogmios
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts)
import Ctl.Internal.Wallet (Wallet)
import Ctl.Internal.Wallet.Spec (WalletSpec)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import MedeaPrelude (class MonadAff)

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

--------------------------------------------------------------------------------
-- ContractEnv
--------------------------------------------------------------------------------

data QueryBackend
  = CtlBackend
      { ogmiosConfig :: ServerConfig
      , ogmiosWs :: OgmiosWebSocket
      , kupoConfig :: ServerConfig
      }
  | BlockfrostBackend
      { blockfrostConfig :: ServerConfig
      }

type ContractEnv =
  { backend :: QueryBackend
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

type ContractRuntime =
  { ogmiosWs :: Maybe OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  , pparams :: Ogmios.ProtocolParameters -- TODO:
  }

{-
-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: Warn
       (  Text "Using `mkContractEnv` is not recommended: it does not ensure"
       <> Text " `ContractEnv` finalization. Consider using `withContractEnv`"
       )
  => ContractParams
  -> Aff ContractEnv
mkContractEnv
  params@
    { backendParams
    , ctlServerConfig
    , datumCacheConfig
    , networkId
    , logLevel
    , walletSpec
    , customLogger
    , suppressLogs
    , hooks
    } =
-}

{- mkContractRuntime :: ContractParams -> Aff ContractRuntime
mkContractRuntime params = do
  for_ params.hooks.beforeInit (void <<< liftEffect <<< try)
  usedTxOuts <- newUsedTxOuts
-}

--------------------------------------------------------------------------------
-- ContractParams
--------------------------------------------------------------------------------

data QueryBackendParams
  = CtlBackendParams
      { ogmiosConfig :: ServerConfig
      , kupoConfig :: ServerConfig
      }
  | BlockfrostBackendParams
      { blockfrostConfig :: ServerConfig
      }

-- | Options to construct a `ContractEnv` indirectly.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can be run in parallel, reusing the same
-- | environment (see `withContractEnv`)
type ContractParams =
  { backendParams :: QueryBackendParams
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

