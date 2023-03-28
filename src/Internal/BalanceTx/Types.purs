module Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , BalanceTxMContext
  , FinalizedTransaction(FinalizedTransaction)
  , askCip30Wallet
  , askCoinsPerUtxoUnit
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftEitherContract
  , liftContract
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
import Ctl.Internal.Contract.Monad (Contract, ContractEnv)
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Types.ProtocolParameters (CoinsPerUtxoUnit)
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
  ExceptT BalanceTxError (ReaderT BalanceTxMContext Contract) a

liftContract :: forall (a :: Type). Contract a -> BalanceTxM a
liftContract = lift <<< lift

liftEitherContract
  :: forall (a :: Type). Contract (Either BalanceTxError a) -> BalanceTxM a
liftEitherContract = ExceptT <<< lift

asksConstraints
  :: forall (a :: Type). Lens' BalanceTxConstraints a -> BalanceTxM a
asksConstraints l = asks (view l <<< _.constraints)

asksContractEnv :: forall (a :: Type). (ContractEnv -> a) -> BalanceTxM a
asksContractEnv = lift <<< lift <<< asks

askCoinsPerUtxoUnit :: BalanceTxM CoinsPerUtxoUnit
askCoinsPerUtxoUnit =
  asksContractEnv
    (_.coinsPerUtxoUnit <<< unwrap <<< _.pparams <<< _.ledgerConstants)

askCip30Wallet :: BalanceTxM (Maybe Cip30Wallet)
askCip30Wallet = asksContractEnv (cip30Wallet <=< _.wallet)

askNetworkId :: BalanceTxM NetworkId
askNetworkId = asksContractEnv _.networkId

withBalanceTxConstraints
  :: forall (a :: Type)
   . BalanceTxConstraintsBuilder
  -> ReaderT BalanceTxMContext Contract a
  -> Contract a
withBalanceTxConstraints constraintsBuilder =
  flip runReaderT { constraints }
  where
  constraints :: BalanceTxConstraints
  constraints = Constraints.buildBalanceTxConstraints constraintsBuilder

askCostModelsForLanguages :: Set Language -> BalanceTxM Costmdls
askCostModelsForLanguages languages =
  asksContractEnv (_.costModels <<< unwrap <<< _.pparams <<< _.ledgerConstants)
    <#> over Costmdls (Map.filterKeys (flip Set.member languages))

newtype FinalizedTransaction = FinalizedTransaction Transaction

derive instance Generic FinalizedTransaction _
derive instance Newtype FinalizedTransaction _
derive newtype instance Eq FinalizedTransaction

instance Show FinalizedTransaction where
  show = genericShow
