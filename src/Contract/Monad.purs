-- | A module defining the `Contract` monad.
module Contract.Monad
  ( Contract(..)
  , ContractConfig(..)
  , ConfigParams(..)
  , DefaultContractConfig
  , module Aff
  , module QueryM
  , module Log.Level
  , module Log.Tag
  , configWithLogLevel
  , defaultTestnetContractConfig
  , liftContractAffM
  , liftContractE
  , liftContractE'
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , logTrace
  , logTrace'
  , logDebug
  , logDebug'
  , logInfo
  , logInfo'
  , logWarn
  , logWarn'
  , logError
  , logError'
  , logAeson
  , logAesonTrace
  , logAesonDebug
  , logAesonInfo
  , logAesonWarn
  , logAesonError
  , mkContractConfig
  , runContract
  , runContract_
  , throwContractError
  , traceTestnetContractConfig
  , wrapContract
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , encodeAeson
  , stringifyAeson
  )
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
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
import Data.Log.Level (LogLevel(Error, Trace))
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error)) as Log.Level
import Data.Log.Message (Message)
import Data.Log.Tag (TagSet)
import Data.Log.Tag
  ( TagSet
  , tag
  , intTag
  , numberTag
  , booleanTag
  , jsDateTag
  , tagSetTag
  ) as Log.Tag
import Data.Map as Map
import Data.Maybe (Maybe(Just), maybe)
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
import QueryM (QueryM, QueryMExtended, QueryConfig)
import QueryM.ProtocolParameters (getProtocolParametersAff) as Ogmios
import Record as Record
import Serialization.Address (NetworkId(TestnetId))
import Types.UsedTxOuts (newUsedTxOuts)
import Wallet (Wallet, mkNamiWalletAff)

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
-- | underlying `QueryConfig`, see `ContractConfig`.
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

instance MonadAsk (ContractConfig r) (Contract r) where
  -- Use the underlying `ask`:
  ask = Contract $ ContractConfig <$> ask

instance MonadReader (ContractConfig r) (Contract r) where
  -- Use the underlying `local` after dimapping and unwrapping:
  local f contract = Contract $ local (dimap wrap unwrap f) (unwrap contract)

-- | The config for `Contract` is just a newtype wrapper over the underlying
-- | `QueryM` config. To use a configuration with default values, see
-- | `defaultContractConfig`. To specify non-default configuration values, it
-- | is recommended to construct a `ContractConfig` indirectly with from
-- | `ConfigParams` using `mkContractConfig`
newtype ContractConfig (r :: Row Type) = ContractConfig (QueryConfig r)

type DefaultContractConfig = ContractConfig ()

derive instance Newtype (ContractConfig r) _

wrapContract :: forall (r :: Row Type) (a :: Type). QueryM a -> Contract r a
wrapContract = wrap <<< QueryM.liftQueryM

-- | Options to construct a `ContractConfig` indirectly. Use `mkContractConfig`
-- | to create a `ContractConfig` which will call the necessary effects for
-- | websocket initializations according to the provided options. `extraConfig`
-- | holds additional options that will extend the resulting `ContractConfig`
-- | directly
newtype ConfigParams (r :: Row Type) = ConfigParams
  { ogmiosConfig :: QueryM.ServerConfig
  , datumCacheConfig :: QueryM.ServerConfig
  , ctlServerConfig :: QueryM.ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , wallet :: Maybe Wallet
  -- | Additional config options to extend the `ContractConfig`
  , extraConfig :: { | r }
  }

derive instance Newtype (ConfigParams r) _

-- | Create a `ContractConfig` from the provided params. This will call the
-- | necessary initialization code for the websocket connections
mkContractConfig
  :: forall (r :: Row Type). ConfigParams r -> Aff (ContractConfig r)
mkContractConfig
  (ConfigParams params@{ logLevel, networkId, wallet }) = do
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff logLevel params.ogmiosConfig
  datumCacheWs <- QueryM.mkDatumCacheWebSocketAff logLevel
    params.datumCacheConfig
  pparams <- Ogmios.getProtocolParametersAff ogmiosWs logLevel
  usedTxOuts <- newUsedTxOuts
  let
    queryConfig =
      { logLevel
      , networkId
      , ogmiosWs
      , usedTxOuts
      , wallet
      , datumCacheWs
      , serverConfig: params.ctlServerConfig
      , pparams
      }
  pure $ wrap $ queryConfig `Record.union` params.extraConfig

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

-- | Runs the contract, essentially `runReaderT` but with arguments flipped.
runContract
  :: forall (r :: Row Type) (a :: Type)
   . ContractConfig r
  -> Contract r a
  -> Aff a
runContract config = flip runLoggerT printLog <<< flip runReaderT cfg <<< unwrap
  where
  printLog :: Message -> Aff Unit
  printLog = logWithLevel cfg.logLevel

  cfg :: QueryConfig r
  cfg = unwrap config

-- | Same as `runContract` discarding output.
runContract_
  :: forall (r :: Row Type) (a :: Type)
   . ContractConfig r
  -> Contract r a
  -> Aff Unit
runContract_ config = void <<< runContract config

-- | Creates a default `ContractConfig` with a Nami wallet inside `Aff` as
-- | required by the websockets.
defaultTestnetContractConfig :: Aff DefaultContractConfig
defaultTestnetContractConfig = do
  wallet <- mkNamiWalletAff
  configWithLogLevel TestnetId wallet Error

-- | Same as `defaultTestnetContractConfig` but with `Trace` config level.
-- | Should be used for testing.
traceTestnetContractConfig :: Aff DefaultContractConfig
traceTestnetContractConfig = do
  wallet <- mkNamiWalletAff
  configWithLogLevel TestnetId wallet Trace

configWithLogLevel
  :: NetworkId -> Wallet -> LogLevel -> Aff DefaultContractConfig
configWithLogLevel networkId wallet logLevel = do
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff logLevel QueryM.defaultOgmiosWsConfig
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff logLevel QueryM.defaultDatumCacheWsConfig
  pparams <- Ogmios.getProtocolParametersAff ogmiosWs logLevel
  usedTxOuts <- newUsedTxOuts
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet: Just wallet
    , usedTxOuts
    , serverConfig: QueryM.defaultServerConfig
    , networkId
    , logLevel
    , pparams
    }

-- Logging effects

logTrace
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logTrace = Logger.trace

logDebug
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logDebug = Logger.debug

logInfo
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logInfo = Logger.info

logWarn
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logWarn = Logger.warn

logError
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logError = Logger.error

logTrace'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logTrace' = Logger.trace Map.empty

logDebug'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logDebug' = Logger.debug Map.empty

logInfo'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logInfo' = Logger.info Map.empty

logWarn'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logWarn' = Logger.warn Map.empty

logError'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logError' = Logger.error Map.empty

-- Log JSON representation of a data structure
logAeson
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => (String -> m Unit)
  -- ^ Logging function to use
  -> a
  -- ^ Data structure to output
  -> m Unit
logAeson logger = logger <<< stringifyAeson <<< encodeAeson

logAesonTrace
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonTrace = logAeson logTrace'

logAesonDebug
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonDebug = logAeson logDebug'

logAesonInfo
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonInfo = logAeson logInfo'

logAesonWarn
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonWarn = logAeson logWarn'

logAesonError
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonError = logAeson logError'
