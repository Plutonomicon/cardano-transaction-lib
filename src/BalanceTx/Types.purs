module BalanceTx.Types
  ( BalanceTxM
  , BalanceTxMContext
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askConstraints
  , liftEitherQueryM
  , liftQueryM
  , withBalanceTxConstraints
  ) where

import Prelude

import BalanceTx.Constraints (BalanceTxConstraints, BalanceTxConstraintsBuilder)
import BalanceTx.Constraints (buildBalanceTxConstraints) as Constraints
import BalanceTx.Error (BalanceTxError)
import Cardano.Types.Transaction (Transaction)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Control.Monad.Reader.Trans (asks, withReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over)
import Data.Show.Generic (genericShow)
import QueryM
  ( DefaultQueryEnv
  , QueryEnv
  , QueryM
  , QueryMExtended(QueryMExtended)
  )
import QueryM (liftQueryM) as QueryM
import Types.ScriptLookups (UnattachedUnbalancedTx)

type BalanceTxMContext = (constraints :: BalanceTxConstraints)

type BalanceTxM (a :: Type) =
  ExceptT BalanceTxError (QueryMExtended BalanceTxMContext) a

askConstraints :: BalanceTxM BalanceTxConstraints
askConstraints = asks (_.constraints <<< _.extraConfig)

liftQueryM :: forall (a :: Type). QueryM a -> BalanceTxM a
liftQueryM = lift <<< QueryM.liftQueryM

liftEitherQueryM
  :: forall (a :: Type). QueryM (Either BalanceTxError a) -> BalanceTxM a
liftEitherQueryM = ExceptT <<< QueryM.liftQueryM

withBalanceTxConstraints
  :: forall (a :: Type)
   . BalanceTxConstraintsBuilder
  -> QueryMExtended BalanceTxMContext a
  -> QueryM a
withBalanceTxConstraints constraintsBuilder =
  over QueryMExtended (withReaderT setQueryEnv)
  where
  setQueryEnv :: DefaultQueryEnv -> QueryEnv BalanceTxMContext
  setQueryEnv defaultEnv =
    let
      constraints :: BalanceTxConstraints
      constraints = Constraints.buildBalanceTxConstraints constraintsBuilder
    in
      defaultEnv { extraConfig = { constraints } }

newtype FinalizedTransaction = FinalizedTransaction Transaction

derive instance Generic FinalizedTransaction _
derive instance Newtype FinalizedTransaction _
derive newtype instance Eq FinalizedTransaction

instance Show FinalizedTransaction where
  show = genericShow

newtype PrebalancedTransaction = PrebalancedTransaction UnattachedUnbalancedTx

derive instance Generic PrebalancedTransaction _
derive instance Newtype PrebalancedTransaction _

instance Show PrebalancedTransaction where
  show = genericShow
