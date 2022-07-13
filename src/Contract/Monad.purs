-- | A module defining the `Contract` monad.
module Contract.Monad
  ( Contract(Contract)
  , ContractEnv(ContractEnv)
  , ConfigParams
  , DefaultContractEnv
  , module Aff
  , module QueryM
  , module Log.Tag
  , liftContractAffM
  , liftContractE
  , liftContractE'
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , runContract
  , runContractInEnv
  , throwContractError
  , withContractEnv
  , wrapContract
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader.Class
  ( class MonadAsk
  , class MonadReader
  , ask
  , local
  )
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either, either, hush)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Log.Tag
  ( TagSet
  , tag
  , intTag
  , numberTag
  , booleanTag
  , jsDateTag
  , tagSetTag
  ) as Log.Tag
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Effect.Aff (Aff)
import Effect.Aff (Aff, launchAff_) as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Helpers (logWithLevel)
import QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , DispatchIdMap
  , Host
  , ListenerSet
  , OgmiosListeners
  , OgmiosWebSocket
  , ServerConfig
  , WebSocket
  , liftQueryM
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , mkDatumCacheWebSocketAff
  , mkHttpUrl
  , mkOgmiosWebSocketAff
  , mkWsUrl
  ) as QueryM
import QueryM
  ( QueryConfig
  , QueryM
  , QueryMExtended
  , QueryRuntime
  , QueryEnv
  , withQueryRuntime
  )
import Serialization.Address (NetworkId)
import Wallet.Spec (WalletSpec)

-- | The `Contract` monad is a newtype wrapper over `QueryM` which is `ReaderT`
-- | on `QueryConfig` over asynchronous effects, `Aff`. Throwing and catching
-- | errors can therefore be implemented with native JavaScript
-- | `Effect.Exception.Error`s and `Effect.Class.Console.log` replaces the
-- | `Writer` monad. `Aff` enables the user to make effectful calls inside this
-- | `Contract` monad.
-- |
-- | The `Contract` has the same capabilities as the underlying `QueryM` but
-- | `Contract` provides a seperation of intent. While the user may access
-- | the underlying type alias, `QueryM`, we intend to keep the mentioned type
-- | internal for all requests to wallets and servers. Although the user may
-- | find the type in the `QueryM` module.
-- |
-- | The configuration for `Contract` is also a newtype wrapper over the
-- | underlying `QueryConfig`, see `ContractEnv`.
-- |
-- | All useful functions written in `QueryM` should be lifted into the
-- | `Contract` monad and available in the same namespace. If anything is
-- | missing, please contact us.
newtype Contract (r :: Row Type) (a :: Type) = Contract (QueryMExtended r a)

-- Many of these derivations of depending on the underlying `ReaderT` and
-- asychronous effects,`Aff`.
derive instance Newtype (Contract r a) _
derive newtype instance Functor (Contract r)
derive newtype instance Apply (Contract r)
derive newtype instance Applicative (Contract r)
derive newtype instance Bind (Contract r)
derive newtype instance Monad (Contract r)
derive newtype instance MonadEffect (Contract r)
derive newtype instance MonadAff (Contract r)
derive newtype instance Semigroup a => Semigroup (Contract r a)
derive newtype instance Monoid a => Monoid (Contract r a)
-- Utilise JavaScript's native `Error` via underlying `Aff` for flexibility:
derive newtype instance MonadThrow Error (Contract r)
derive newtype instance MonadError Error (Contract r)
derive newtype instance MonadRec (Contract r)
derive newtype instance MonadLogger (Contract r)

instance MonadAsk (ContractEnv r) (Contract r) where
  -- Use the underlying `ask`:
  ask = Contract $ ContractEnv <$> ask

instance MonadReader (ContractEnv r) (Contract r) where
  -- Use the underlying `local` after dimapping and unwrapping:
  local f contract = Contract $ local (dimap wrap unwrap f) (unwrap contract)

-- | `ContractEnv` is a trivial wrapper over `QueryEnv`.
newtype ContractEnv (r :: Row Type) = ContractEnv (QueryEnv r)

type DefaultContractEnv = ContractEnv ()

derive instance Newtype (ContractEnv r) _

wrapContract :: forall (r :: Row Type) (a :: Type). QueryM a -> Contract r a
wrapContract = wrap <<< QueryM.liftQueryM

-- | Options to construct a `ContractEnv` indirectly. `extraConfig`
-- | holds additional options that will extend the resulting `ContractEnv`.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can be run in parallel, reusing the same
-- | environment (see `withContractEnv`)
type ConfigParams (r :: Row Type) =
  { ogmiosConfig :: QueryM.ServerConfig
  , datumCacheConfig :: QueryM.ServerConfig
  , ctlServerConfig :: QueryM.ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (Message -> Aff Unit)
  -- | Additional config options to extend the `ContractEnv`
  , extraConfig :: { | r }
  }

-- | Interprets a contract into an `Aff` context.
-- | Implicitly initializes and finalizes a new `ContractEnv` runtime.
-- |
-- | Use `withContractEnv` if your application contains multiple contracts that
-- | can be run in parallel, reusing the same environment (see
-- | `withContractEnv`)
runContract
  :: forall (r :: Row Type) (a :: Type)
   . ConfigParams r
  -> Contract r a
  -> Aff a
runContract params contract = do
  withContractEnv params \config ->
    runContractInEnv config contract

-- | Runs a contract in existing environment. Does not destroy the environment
-- | when contract execution ends.
runContractInEnv
  :: forall (r :: Row Type) (a :: Type)
   . ContractEnv r
  -> Contract r a
  -> Aff a
runContractInEnv config =
  flip runLoggerT printLog
    <<< flip runReaderT (unwrap config)
    <<< unwrap
  where
  printLog :: Message -> Aff Unit
  printLog = logWithLevel (unwrap config).config.logLevel

-- | Constructs and finalizes a contract environment that is usable inside a
-- | bracket callback.
-- | Make sure that `Aff` action does not end before all contracts that use the
-- | runtime terminate. Otherwise `WebSocket`s will be closed too early.
withContractEnv
  :: forall (r :: Row Type) (a :: Type)
   . ConfigParams r
  -> (ContractEnv r -> Aff a)
  -> Aff a
withContractEnv
  params@
    { ctlServerConfig
    , ogmiosConfig
    , datumCacheConfig
    , networkId
    , logLevel
    , walletSpec
    , customLogger
    }
  action = do
  let
    config =
      { ctlServerConfig
      , ogmiosConfig
      , datumCacheConfig
      , networkId
      , logLevel
      , walletSpec
      , customLogger
      }
  withQueryRuntime config \runtime -> do
    let
      contractSettings = wrap
        { runtime, config, extraConfig: params.extraConfig }
    action contractSettings

-- | Throws an `Error` for any showable error using `Effect.Exception.throw`
-- | and lifting into the `Contract` monad.
throwContractError
  :: forall (e :: Type) (r :: Row Type) (a :: Type). Show e => e -> Contract r a
throwContractError = liftEffect <<< throw <<< show

-- | Given a string error and `Maybe` value, if the latter is `Nothing`, throw
-- | the error with the given string, otherwise, return the value. If using
-- | using `runExceptT`, see `liftM` inside `Contract.Prelude`. This can be
-- | thought of as `liftM` restricted to JavaScript's `Error` and without the
-- | need to call `error :: String -> Error` each time.
liftContractM
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Maybe a
  -> Contract r a
liftContractM str = maybe (liftEffect $ throw str) pure

-- | Same as `liftContractM` but the `Maybe` value is already in the `Contract`
-- | context.
liftedM
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Contract r (Maybe a)
  -> Contract r a
liftedM str cm = cm >>= liftContractM str

-- | Same as `liftContractM` but the `Maybe` value is in the `Aff` context.
liftContractAffM
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Aff (Maybe a)
  -> Contract r a
liftContractAffM str = liftedM str <<< liftAff

-- | Similar to `liftContractE` except it directly throws the showable error
-- | via `throwContractError` instead of an arbitrary string.
liftContractE
  :: forall (e :: Type) (r :: Row Type) (a :: Type)
   . Show e
  => Either e a
  -> Contract r a
liftContractE = either throwContractError pure

-- | Similar to `liftContractM`, throwing the string instead of the `Left`
-- | value. For throwing the `Left` value, see `liftEither` in
-- | `Contract.Prelude`.
liftContractE'
  :: forall (e :: Type) (r :: Row Type) (a :: Type)
   . String
  -> Either e a
  -> Contract r a
liftContractE' str = liftContractM str <<< hush

-- | Similar to `liftedE` except it directly throws the showable error via
-- | `throwContractError` instead of an arbitrary string.
liftedE
  :: forall (e :: Type) (r :: Row Type) (a :: Type)
   . Show e
  => Contract r (Either e a)
  -> Contract r a
liftedE = (=<<) liftContractE

-- | Same as `liftContractE` but the `Either` value is already in the `Contract`
-- | context.
liftedE'
  :: forall (e :: Type) (r :: Row Type) (a :: Type)
   . String
  -> Contract r (Either e a)
  -> Contract r a
liftedE' str em = em >>= liftContractE' str
