module Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , BalanceTxMContext
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCip30Wallet
  , askCoinsPerUtxoUnit
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftEitherQueryM
  , liftQueryM
  , withBalanceTxConstraints
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraints
  , BalanceTxConstraintsBuilder
  )
import Ctl.Internal.BalanceTx.Constraints
  ( buildBalanceTxConstraints
  ) as Constraints
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.Cardano.Types.Transaction (Costmdls(Costmdls), Transaction)
import Ctl.Internal.QueryM (QueryEnv, QueryM)
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Types.Scripts (Language)
import Ctl.Internal.Wallet (Cip30Wallet, cip30Wallet)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Getter (view)
import Data.Map (filterKeys) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Set (member) as Set
import Data.Show.Generic (genericShow)

type BalanceTxMContext = { constraints :: BalanceTxConstraints }

type BalanceTxM (a :: Type) =
  ExceptT BalanceTxError (ReaderT BalanceTxMContext QueryM) a

liftQueryM :: forall (a :: Type). QueryM a -> BalanceTxM a
liftQueryM = lift <<< lift

liftEitherQueryM
  :: forall (a :: Type). QueryM (Either BalanceTxError a) -> BalanceTxM a
liftEitherQueryM = ExceptT <<< lift

asksConstraints
  :: forall (a :: Type). Lens' BalanceTxConstraints a -> BalanceTxM a
asksConstraints l = asks (view l <<< _.constraints)

asksQueryEnv :: forall (a :: Type). (QueryEnv () -> a) -> BalanceTxM a
asksQueryEnv = lift <<< lift <<< asks

askCoinsPerUtxoUnit :: BalanceTxM CoinsPerUtxoUnit
askCoinsPerUtxoUnit =
  asksQueryEnv (_.coinsPerUtxoUnit <<< unwrap <<< _.pparams <<< _.runtime)

askCip30Wallet :: BalanceTxM (Maybe Cip30Wallet)
askCip30Wallet = asksQueryEnv (cip30Wallet <=< _.wallet <<< _.runtime)

askNetworkId :: BalanceTxM NetworkId
askNetworkId = asksQueryEnv (_.networkId <<< _.config)

withBalanceTxConstraints
  :: forall (a :: Type)
   . BalanceTxConstraintsBuilder
  -> ReaderT BalanceTxMContext QueryM a
  -> QueryM a
withBalanceTxConstraints constraintsBuilder =
  flip runReaderT { constraints }
  where
  constraints :: BalanceTxConstraints
  constraints = Constraints.buildBalanceTxConstraints constraintsBuilder

askCostModelsForLanguages :: Set Language -> BalanceTxM Costmdls
askCostModelsForLanguages languages =
  asksQueryEnv (_.costModels <<< unwrap <<< _.pparams <<< _.runtime)
    <#> over Costmdls (Map.filterKeys (flip Set.member languages))

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
