module Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , BalanceTxMContext
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCoinsPerUtxoUnit
  , askConstraints
  , liftEitherQueryM
  , liftQueryM
  , withBalanceTxConstraints
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Control.Monad.Reader.Trans (asks, withReaderT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraints
  , BalanceTxConstraintsBuilder
  )
import Ctl.Internal.BalanceTx.Constraints
  ( buildBalanceTxConstraints
  ) as Constraints
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.Cardano.Types.Transaction (Transaction)
import Ctl.Internal.QueryM
  ( DefaultQueryEnv
  , QueryEnv
  , QueryM
  , QueryMExtended(QueryMExtended)
  )
import Ctl.Internal.QueryM (liftQueryM) as QueryM
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)

type BalanceTxMContext = (constraints :: BalanceTxConstraints)

type BalanceTxM (a :: Type) =
  ExceptT BalanceTxError (QueryMExtended BalanceTxMContext Aff) a

askCoinsPerUtxoUnit :: BalanceTxM CoinsPerUtxoUnit
askCoinsPerUtxoUnit =
  asks (_.coinsPerUtxoUnit <<< unwrap <<< _.pparams <<< _.runtime)

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
  -> QueryMExtended BalanceTxMContext Aff a
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
