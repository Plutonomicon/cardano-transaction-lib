-- | A module defining the `Contract` monad.
module Contract.Monad
  ( Contract(..)
  , module QueryM
  , runContract
  , runContract_
  , throwError
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import QueryM (QueryM)
import QueryM (QueryConfig) as QueryM

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
derive newtype instance Semigroup a => Semigroup (Contract a)
derive newtype instance Monoid a => Monoid (Contract a)
-- Utilise JavaScript's native `Error` via underlying `Aff` for flexibility:
derive newtype instance MonadThrow Error Contract
derive newtype instance MonadError Error Contract
derive newtype instance MonadAsk QueryM.QueryConfig Contract
derive newtype instance MonadReader QueryM.QueryConfig Contract
derive newtype instance MonadRec Contract

-- | Throws an `Error` for any showable error.
throwError :: forall (e :: Type). Show e => e -> Contract e
throwError = liftEffect <<< throw <<< show

-- | Runs the contract, essentially `runReaderT` but with arguments flipped.
runContract :: forall (a :: Type). QueryM.QueryConfig -> Contract a -> Aff a
runContract config = flip runReaderT config <<< unwrap

-- | Same as `runContract` discarding output.
runContract_ :: forall (a :: Type). QueryM.QueryConfig -> Contract a -> Aff Unit
runContract_ config = void <<< flip runReaderT config <<< unwrap
