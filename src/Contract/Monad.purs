-- | A module defining the `Contract` monad.
module Contract.Monad
  ( Contract(..)
  , ContractConfig(..)
  , defaultContractConfig
  , defaultContractConfigAff
  , runContract
  , runContract_
  , throwContractError
  , module QueryM
  ) where

import Prelude
import Control.Alt (class Alt)
import Data.Maybe (Maybe(Just))
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
import Data.Profunctor (dimap)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import QueryM (QueryM, QueryConfig)
import QueryM
  ( DatumCacheListeners
  , DatumCacheWebSocket
  , DispatchIdMap
  , Host
  , JsWebSocket
  , ListenerSet
  , OgmiosListeners
  , OgmiosWebSocket
  , ServerConfig
  , WebSocket
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , mkDatumCacheWebSocketAff
  , mkHttpUrl
  , mkOgmiosWebSocketAff
  , mkWsUrl
  ) as QueryM
import UsedTxOuts (newUsedTxOuts)
import Wallet (mkNamiWalletAff)

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
newtype Contract (a :: Type) = Contract (QueryM a)

-- Many of these derivations of depending on the underlying `ReaderT` and
-- asychronous effects,`Aff`.
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
-- Utilise JavaScript's native `Error` via underlying `Aff` for flexibility:
derive newtype instance MonadThrow Error Contract
derive newtype instance MonadError Error Contract
derive newtype instance MonadRec Contract

instance MonadAsk ContractConfig Contract where
  -- Use the underlying `ask`:
  ask = Contract $ ContractConfig <$> ask

instance MonadReader ContractConfig Contract where
  -- Use the underlying `local` after dimapping and unwrapping:
  local f contract = Contract $ local (dimap wrap unwrap f) (unwrap contract)

-- | The config for `Contract` is just a newtype wrapper over the underlying
-- | `QueryM` config.
newtype ContractConfig = ContractConfig QueryConfig

derive instance Newtype ContractConfig _

-- | Throws an `Error` for any showable error using `Effect.Exception.throw`
-- | and lifting into the `Contract` monad.
throwContractError :: forall (e :: Type) (a :: Type). Show e => e -> Contract a
throwContractError = liftEffect <<< throw <<< show

-- | Runs the contract, essentially `runReaderT` but with arguments flipped.
runContract :: forall (a :: Type). ContractConfig -> Contract a -> Aff a
runContract config = flip runReaderT (unwrap config) <<< unwrap

-- | Same as `runContract` discarding output.
runContract_ :: forall (a :: Type). ContractConfig -> Contract a -> Aff Unit
runContract_ config = void <<< flip runReaderT (unwrap config) <<< unwrap

-- | Creates a default `ContractConfig` with a Nami wallet inside `Aff` as
-- | required by the websockets.
defaultContractConfigAff :: Aff ContractConfig
defaultContractConfigAff = do
  wallet <- Just <$> mkNamiWalletAff
  ogmiosWs <- QueryM.mkOgmiosWebSocketAff QueryM.defaultOgmiosWsConfig
  datumCacheWs <-
    QueryM.mkDatumCacheWebSocketAff QueryM.defaultDatumCacheWsConfig
  usedTxOuts <- newUsedTxOuts
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet
    , serverConfig: QueryM.defaultServerConfig
    , usedTxOuts
    }

-- | Same as `defaultContractConfigAff` but lifted into `Contract`.
defaultContractConfig :: Contract ContractConfig
defaultContractConfig = liftAff defaultContractConfigAff

