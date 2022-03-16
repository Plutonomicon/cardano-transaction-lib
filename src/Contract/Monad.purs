module Contract.Monad
  ( Contract(..)
  , module QueryM
  ) where

import Prelude
-- import BalanceTx (BalanceTxError)
import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
-- import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
-- import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect) -- , liftEffect)
import Effect.Exception (Error) -- , throw)
import QueryM (QueryM)
import QueryM (QueryConfig) as QueryM

-- import Types.ScriptLookups (MkUnbalancedTxError)

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
-- TO DO: can we define MonadError for `ContractError`? `Error` below come's from the `Aff`
-- Monad, is it possible to derive a `ContractError` error type without
-- using `Contract (ExceptT ContractError QueryM)`? Or would we rather `Error`
-- to make use of Javascript native erroring.
derive newtype instance MonadThrow Error Contract
derive newtype instance MonadError Error Contract
derive newtype instance MonadAsk QueryM.QueryConfig Contract
derive newtype instance MonadReader QueryM.QueryConfig Contract
derive newtype instance MonadRec Contract

-- data ContractError
--   = ConstrantResolutionError MkUnbalancedTxError
--   | OtherError String
--   | BalanceError BalanceTxError

-- derive instance Generic ContractError _

-- instance Show ContractError where
--   show = genericShow

-- Custom MonadThrow:
-- instance MonadThrow ContractError Contract where
--   throwError = liftEffect <<< throw <<< show
