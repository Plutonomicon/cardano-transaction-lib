module Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , BalanceTxMContext
  , askCoinsPerUtxoUnit
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , liftEitherContract
  , liftContract
  , withBalancerConstraints
  ) where

import Prelude

import Cardano.Types (Address, Coin, CostModel, Language, NetworkId)
import Control.Monad.Except.Trans (ExceptT(ExceptT))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.Constraints
  ( BalancerConfig
  , BalancerConstraints
  , buildBalancerConfig
  )
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.Contract.Monad (Contract, ContractEnv)
import Ctl.Internal.Contract.Wallet (getWalletAddresses)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Getter (view)
import Data.Map (Map)
import Data.Map (filterKeys) as Map
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (fromFoldable, member) as Set

type BalanceTxMContext =
  { constraints :: BalancerConfig, ownAddresses :: Set Address }

type BalanceTxM (a :: Type) =
  ExceptT BalanceTxError (ReaderT BalanceTxMContext Contract) a

liftContract :: forall (a :: Type). Contract a -> BalanceTxM a
liftContract = lift <<< lift

liftEitherContract
  :: forall (a :: Type). Contract (Either BalanceTxError a) -> BalanceTxM a
liftEitherContract = ExceptT <<< lift

asksConstraints
  :: forall (a :: Type). Lens' BalancerConfig a -> BalanceTxM a
asksConstraints l = asks (view l <<< _.constraints)

asksContractEnv :: forall (a :: Type). (ContractEnv -> a) -> BalanceTxM a
asksContractEnv = lift <<< lift <<< asks

askCoinsPerUtxoUnit :: BalanceTxM Coin
askCoinsPerUtxoUnit =
  asksContractEnv
    (_.coinsPerUtxoByte <<< unwrap <<< _.pparams <<< _.ledgerConstants)

askNetworkId :: BalanceTxM NetworkId
askNetworkId = asksContractEnv _.networkId

withBalancerConstraints
  :: forall (a :: Type)
   . BalancerConstraints
  -> ReaderT BalanceTxMContext Contract a
  -> Contract a
withBalancerConstraints constraintsBuilder m = do
  -- we can ignore failures due to reward addresses because reward addresses
  -- do not receive transaction outputs from dApps
  ownAddresses <- Set.fromFoldable <$> getWalletAddresses
  flip runReaderT { constraints, ownAddresses } m
  where
  constraints :: BalancerConfig
  constraints = buildBalancerConfig constraintsBuilder

askCostModelsForLanguages :: Set Language -> BalanceTxM (Map Language CostModel)
askCostModelsForLanguages languages =
  asksContractEnv (_.costModels <<< unwrap <<< _.pparams <<< _.ledgerConstants)
    <#> Map.filterKeys (flip Set.member languages)
