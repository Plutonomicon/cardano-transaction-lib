-- | A module defining the `Contract` monad.
module Contract.Monad
  ( Contract(Contract)
  , ParContract
  , ContractEnv(ContractEnv)
  , ConfigParams
  , DefaultContractEnv
  , module Aff
  , module QueryM
  , module Log.Tag
  , askConfig
  , asksConfig
  , liftContractAffM
  , liftContractE
  , liftContractE'
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , mkContractEnv
  , runContract
  , runContractInEnv
  , stopContractEnv
  , throwContractError
  , withContractEnv
  , wrapContract
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , catchError
  )
import Control.Monad.Except (throwError)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class
  ( class MonadAsk
  , class MonadReader
  , ask
  , asks
  , local
  )
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus, empty)
import Ctl.Internal.QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , Host
  , ListenerSet
  , Logger
  , OgmiosListeners
  , OgmiosWebSocket
  , QueryConfig
  , QueryEnv
  , QueryM
  , QueryMExtended
  , QueryRuntime
  , ServerConfig
  , WebSocket
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , liftQueryM
  , mkDatumCacheWebSocketAff
  , mkHttpUrl
  , mkLogger
  , mkOgmiosWebSocketAff
  , mkWsUrl
  ) as QueryM
import Ctl.Internal.QueryM
  ( Hooks
  , QueryConfig
  , QueryEnv
  , QueryM
  , QueryMExtended
  , ServerConfig
  , liftQueryM
  , mkQueryRuntime
  , stopQueryRuntime
  , withQueryRuntime
  )
import Ctl.Internal.QueryM.Logging (setupLogs)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Wallet.Spec (WalletSpec)
import Data.Either (Either(Left, Right), either, hush)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Log.Tag
  ( TagSet
  , booleanTag
  , intTag
  , jsDateTag
  , numberTag
  , tag
  , tagSetTag
  ) as Log.Tag
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, try)
import Effect.Aff (Aff, launchAff_) as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Prim.TypeError (class Warn, Text)

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
newtype Contract (r :: Row Type) (a :: Type) = Contract (QueryMExtended r Aff a)

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

instance Parallel (ParContract r) (Contract r) where
  parallel :: Contract r ~> ParContract r
  parallel = ParContract <<< parallel <<< unwrap
  sequential :: ParContract r ~> Contract r
  sequential (ParContract a) = wrap $ sequential a

-- | The `ParContract` applicative is a newtype wrapper over `ParQueryM`, which
-- | is a `ReaderT` on `QueryConfig` over _parallel_ asynchronous effects,
-- | `ParAff`. As expected, it's `Applicative` instance combines effects in
-- | parallel. It's similar in functionality to `Contract`, but it notably lacks
-- | a `Monad` instance and all the associated monadic functionalities
-- | (like `MonadThrow`, `MonadError`, etc). This datatype is a requirement for
-- | the `Parallel` instance that `Contract` contains. As such, there is little
-- | point in using it directly, since all the methods in `Control.Parallel`
-- | can be run directly on `Contract`. Also, there are no functions for
-- | directly executing a `ParContract`; all `ParContract`s must eventually
-- | be converted to `Contract`s before execution.
newtype ParContract (r :: Row Type) (a :: Type) = ParContract
  (QueryMExtended r ParAff a)

derive newtype instance Functor (ParContract r)
derive newtype instance Apply (ParContract r)
derive newtype instance Applicative (ParContract r)
derive newtype instance Alt (ParContract r)
derive newtype instance Plus (ParContract r)
derive newtype instance Alternative (ParContract r)
derive newtype instance Semigroup a => Semigroup (ParContract r a)
derive newtype instance Monoid a => Monoid (ParContract r a)

-- | `ContractEnv` is a trivial wrapper over `QueryEnv`.
newtype ContractEnv (r :: Row Type) = ContractEnv (QueryEnv r)

-- | Contract's `Alt` instance piggie-backs on the underlying `Aff`'s `Alt`
-- | instance, which uses `MonadError` capabilities.
-- | You can use `alt` operator to provide an alternative contract in case
-- | the first one fails with an error.
instance Alt (Contract r) where
  alt a1 a2 = catchError a1 (const a2)

-- | Identity for `alt` - a.k.a. a contract that "always fails".
instance Plus (Contract r) where
  empty = liftAff empty

type DefaultContractEnv = ContractEnv ()

derive instance Newtype (ContractEnv r) _

wrapContract :: forall (r :: Row Type) (a :: Type). QueryM a -> Contract r a
wrapContract = wrap <<< liftQueryM

-- | Same as `ask`, but points to the user config record.
askConfig
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "User-defined configs are deprecated - https://github.com/Plutonomicon/cardano-transaction-lib/issues/734"
       )
  => Contract r { | r }
askConfig = do
  asks $ unwrap >>> _.extraConfig

-- | Same as `asks`, but allows to apply a function to the user config record.
asksConfig
  :: forall (r :: Row Type) (a :: Type)
   . Warn
       ( Text
           "User-defined configs are deprecated - https://github.com/Plutonomicon/cardano-transaction-lib/issues/734"
       )
  => ({ | r } -> a)
  -> Contract r a
asksConfig f = do
  asks $ unwrap >>> _.extraConfig >>> f

-- | Options to construct a `ContractEnv` indirectly. `extraConfig`
-- | holds additional options that will extend the resulting `ContractEnv`.
-- |
-- | Use `runContract` to run a `Contract` within an implicity constructed
-- | `ContractEnv` environment, or use `withContractEnv` if your application
-- | contains multiple contracts that can be run in parallel, reusing the same
-- | environment (see `withContractEnv`)
type ConfigParams (r :: Row Type) =
  { ogmiosConfig :: ServerConfig
  , datumCacheConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  -- | Suppress logs until an exception is thrown
  , suppressLogs :: Boolean
  -- | Additional config options to extend the `ContractEnv`
  , extraConfig :: { | r }
  , hooks :: Hooks
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
runContractInEnv env contract =
  flip runReaderT (unwrap env)
    $ unwrap
    $ unwrap contract

-- | Initializes a `Contract` environment. Does not ensure finalization.
-- | Consider using `withContractEnv` if possible - otherwise use
-- | `stopContractEnv` to properly finalize.
mkContractEnv
  :: forall (r :: Row Type) (a :: Type)
   . Warn
       ( Text
           "Using `mkContractEnv` is not recommended: it does not ensure `ContractEnv` finalization. Consider using `withContractEnv`"
       )
  => ConfigParams r
  -> Aff (ContractEnv r)
mkContractEnv
  params@
    { ogmiosConfig
    , datumCacheConfig
    , kupoConfig
    , networkId
    , logLevel
    , walletSpec
    , customLogger
    , suppressLogs
    , hooks
    } = do
  let
    config =
      { ogmiosConfig
      , datumCacheConfig
      , kupoConfig
      , networkId
      , logLevel
      , walletSpec
      , customLogger
      , suppressLogs
      , hooks
      }
  runtime <- mkQueryRuntime
    config
  let
    contractEnv = wrap
      { runtime, config, extraConfig: params.extraConfig }
  pure contractEnv

-- | Finalizes a `Contract` environment.
stopContractEnv
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "Using `stopContractEnv` is not recommended: users should rely on `withContractEnv` to finalize the runtime environment instead"
       )
  => ContractEnv r
  -> Effect Unit
stopContractEnv env = stopQueryRuntime (unwrap env).runtime

-- | Constructs and finalizes a contract environment that is usable inside a
-- | bracket callback.
-- | One environment can be used by multiple `Contract`s in parallel (see
-- | `runContractInEnv`).
-- | Make sure that `Aff` action does not end before all contracts that use the
-- | runtime terminate. Otherwise `WebSocket`s will be closed too early.
withContractEnv
  :: forall (r :: Row Type) (a :: Type)
   . ConfigParams r
  -> (ContractEnv r -> Aff a)
  -> Aff a
withContractEnv
  params@
    { ogmiosConfig
    , datumCacheConfig
    , kupoConfig
    , networkId
    , logLevel
    , walletSpec
    , customLogger
    , suppressLogs
    , hooks
    }
  action = do
  { addLogEntry, printLogs } <-
    liftEffect $ setupLogs params.logLevel params.customLogger
  let
    config :: QueryConfig
    config =
      { ogmiosConfig
      , datumCacheConfig
      , kupoConfig
      , networkId
      , logLevel
      , walletSpec
      , customLogger:
          if suppressLogs then Just $ map liftEffect <<< addLogEntry
          else customLogger
      , suppressLogs
      , hooks
      }

  withQueryRuntime config \runtime -> do
    let
      contractEnv = wrap
        { runtime, config, extraConfig: params.extraConfig }
    res <- try $ action contractEnv
    case res of
      Left err -> liftEffect do
        when suppressLogs printLogs
        throwError err
      Right result -> pure result

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
