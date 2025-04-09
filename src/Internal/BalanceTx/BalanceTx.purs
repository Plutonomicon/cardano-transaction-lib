module Ctl.Internal.BalanceTx
  ( CtlBalancer
  , CtlBalancerContext
  , defaultBalancer
  , defaultBalancerErr
  , emptyBalancerCtx
  ) where

import Prelude

import Cardano.Transaction.Balancer (runBalancerAff)
import Cardano.Transaction.Balancer.Constraints
  ( BalancerConstraints
  , buildBalancerConfig
  )
import Cardano.Transaction.Balancer.Error
  ( BalanceTxError
  , explainBalanceTxError
  )
import Cardano.Types (UtxoMap)
import Contract.Log (logInfo')
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.BalanceTx.Sync (isCip30Wallet, syncBackendWithWallet) as Sync
import Ctl.Internal.Contract.Monad
  ( Contract
  , filterLockedUtxos
  , runContractInEnv
  )
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getWalletAddresses
  , getWalletCollateral
  , getWalletUtxos
  ) as Wallet
import Ctl.Internal.Types.TxBalancer (TxBalancer)
import Data.Bifunctor (lmap)
import Data.Map (empty) as Map
import Data.Maybe (isNothing)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error, error)

type CtlBalancerContext =
  { balancerConstraints :: BalancerConstraints
  , extraUtxos :: UtxoMap
  }

emptyBalancerCtx :: CtlBalancerContext
emptyBalancerCtx =
  { balancerConstraints: mempty
  , extraUtxos: Map.empty
  }

type CtlBalancer (err :: Type) = TxBalancer Contract err CtlBalancerContext

defaultBalancer :: CtlBalancer Error
defaultBalancer transaction =
  map (lmap (error <<< explainBalanceTxError))
    <<< defaultBalancerErr transaction

-- | Balances an unbalanced transaction using the specified balancer
-- | constraints.
defaultBalancerErr :: CtlBalancer BalanceTxError
defaultBalancerErr transaction ctx = do
  contractEnv <- ask
  isCip30Wallet <- Sync.isCip30Wallet
  ownAddresses <- Wallet.getWalletAddresses
  let balancerConstraints = buildBalancerConfig ctx.balancerConstraints
  when
    ( isNothing (unwrap balancerConstraints).srcAddresses &&
        contractEnv.synchronizationParams.syncBackendWithWallet.beforeBalancing
    )
    do
      logInfo' "balanceTxWithConstraints: syncBackendWithWallet"
      Sync.syncBackendWithWallet
  liftAff $ runBalancerAff contractEnv.logLevel contractEnv.customLogger
    transaction
    { balancerConstraints
    , provider: contractEnv.provider
    , pparams: unwrap contractEnv.ledgerConstants.pparams
    , network: contractEnv.networkId
    , walletInterface:
        { isCip30Wallet
        , ownAddresses
        , getWalletUtxos: runContractInEnv contractEnv Wallet.getWalletUtxos
        , filterLockedUtxos: runContractInEnv contractEnv <<< filterLockedUtxos
        , getChangeAddress: runContractInEnv contractEnv Wallet.getChangeAddress
        , getWalletCollateral: runContractInEnv contractEnv
            Wallet.getWalletCollateral
        }
    , extraUtxos: ctx.extraUtxos
    }
