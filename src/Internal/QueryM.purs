-- | CTL query layer monad.
-- | This module defines an Aff interface for backend queries.
module Ctl.Internal.QueryM
  ( QueryM
  , QueryEnv
  , QueryConfig
  , ClusterSetup
  , ParQueryM
  , QueryMT(QueryMT)
  , handleAffjaxResponse
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Affjax (Error, Response) as Affjax
import Cardano.Provider.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceOtherError)
  )
import Cardano.Wallet.Key (PrivatePaymentKey, PrivateStakeKey)
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus)
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.QueryM.HttpUtils (handleAffjaxResponseGeneric)
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Either (Either)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

-- | Cluster setup contains everything that is needed to run a `Contract` on
-- | a local cluster: paramters to connect to the services and private keys
-- | that are pre-funded with Ada on that cluster
type ClusterSetup =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , keys ::
      { payment :: PrivatePaymentKey
      , stake :: Maybe PrivateStakeKey
      }
  }

-- | `QueryConfig` contains a complete specification on how to initialize a
-- | `QueryM` environment.
-- | It includes:
-- | - server parameters for all the services
-- | - network ID
-- | - logging level
-- | - optional custom logger
type QueryConfig =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  }

-- | `QueryEnv` contains everything needed for `QueryM` to run.
type QueryEnv =
  { config :: QueryConfig
  }

type QueryM = QueryMT Aff

type ParQueryM = QueryMT ParAff

newtype QueryMT (m :: Type -> Type) (a :: Type) =
  QueryMT (ReaderT QueryEnv m a)

derive instance Newtype (QueryMT m a) _
derive newtype instance Functor m => Functor (QueryMT m)
derive newtype instance Apply m => Apply (QueryMT m)
derive newtype instance Applicative m => Applicative (QueryMT m)
derive newtype instance Bind m => Bind (QueryMT m)
derive newtype instance Alt m => Alt (QueryMT m)
derive newtype instance Plus m => Plus (QueryMT m)
derive newtype instance Alternative m => Alternative (QueryMT m)
derive newtype instance Monad (QueryMT Aff)
derive newtype instance MonadEffect (QueryMT Aff)
derive newtype instance MonadAff (QueryMT Aff)
derive newtype instance
  ( Semigroup a
  , Apply m
  ) =>
  Semigroup (QueryMT m a)

derive newtype instance
  ( Monoid a
  , Applicative m
  ) =>
  Monoid (QueryMT m a)

derive newtype instance MonadThrow Error (QueryMT Aff)
derive newtype instance MonadError Error (QueryMT Aff)
derive newtype instance MonadRec (QueryMT Aff)
derive newtype instance MonadAsk QueryEnv (QueryMT Aff)
derive newtype instance MonadReader QueryEnv (QueryMT Aff)

instance MonadLogger (QueryMT Aff) where
  log msg = do
    config <- asks $ _.config
    let
      logFunction =
        config # _.customLogger >>> fromMaybe logWithLevel
    liftAff $ logFunction config.logLevel msg

-- Newtype deriving complains about overlapping instances, so we wrap and
-- unwrap manually
instance Parallel (QueryMT ParAff) (QueryMT Aff) where
  parallel :: QueryMT Aff ~> QueryMT ParAff
  parallel = wrap <<< parallel <<< unwrap
  sequential :: QueryMT ParAff ~> QueryMT Aff
  sequential = wrap <<< sequential <<< unwrap

handleAffjaxResponse
  :: forall (result :: Type)
   . DecodeAeson result
  => Either Affjax.Error (Affjax.Response String)
  -> Either ClientError result
handleAffjaxResponse =
  handleAffjaxResponseGeneric
    { httpError: ClientHttpError
    , httpStatusCodeError: \statusCode body -> ClientHttpResponseError
        (wrap statusCode)
        (ServiceOtherError body)
    , decodeError: ClientDecodeJsonError
    , parse: (decodeAeson <=< parseJsonStringToAeson)
    , transform: pure
    }
