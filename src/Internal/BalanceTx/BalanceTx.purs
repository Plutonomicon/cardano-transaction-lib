module Ctl.Internal.BalanceTx
  ( balanceTxWithConstraints
  ) where

import Prelude

import Cardano.Transaction.Balancer
  ( getCertsBalance
  , getProposalsBalance
  , runBalancer
  , setTransactionCollateral
  )
import Cardano.Transaction.Balancer.Constraints (BalanceTxConstraintsBuilder)
import Cardano.Transaction.Balancer.Constraints
  ( _changeAddress
  , _changeDatum
  , _selectionStrategy
  , _srcAddresses
  ) as Constraints
import Cardano.Transaction.Balancer.Contract (withBalancerConstraints)
import Cardano.Transaction.Balancer.Error (BalanceTxError(CouldNotGetUtxos))
import Cardano.Transaction.Balancer.Types
  ( BalanceTxM
  , askNetworkId
  , asksConstraints
  , logWithLevel
  , logWithLevelAndTags
  )
import Cardano.Types
  ( Transaction
  , TransactionOutput
  , UtxoMap
  , _body
  , _networkId
  , _witnessSet
  )
import Cardano.Types.Address (Address)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionWitnessSet (_redeemers)
import Cardano.Types.UtxoMap (pprintUtxoMap)
import Contract.Monad (runContractInEnv)
import Contract.Wallet (getWalletAddresses, getWalletCollateral)
import Control.Monad.Except.Trans (except, runExceptT)
import Control.Monad.Reader.Class (ask, asks)
import Control.Parallel (parTraverse)
import Ctl.Internal.BalanceTx.Sync
  ( isCip30Wallet
  , syncBackendWithWallet
  )
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.Wallet (getWalletUtxos) as Wallet
import Data.Array as Array
import Data.Either (Either, hush, note)
import Data.Foldable (foldr)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((?~))
import Data.Log.Level (LogLevel(Info))
import Data.Map (Map)
import Data.Map (empty, union) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

-- | Balances an unbalanced transaction using the specified balancer
-- | constraints.
balanceTxWithConstraints
  :: Transaction
  -> Aff Address
  -> (UtxoMap → Aff UtxoMap)
  -> Map TransactionInput TransactionOutput
  -> BalanceTxConstraintsBuilder
  -> Contract (Either BalanceTxError Transaction)
balanceTxWithConstraints
  transaction
  getChangeAddressAff
  filterLockedUtxosAff
  extraUtxos
  constraintsBuilder = do

  ownAddresses <- getWalletAddresses
  networkId <- asks _.networkId
  protocolParameters <- asks (_.pparams <<< _.ledgerConstants)
  provider <- asks _.provider
  logLevel <- asks _.logLevel
  customLogger <- asks _.customLogger

  syncBeforeBalancing <-
    ( asks $ _.synchronizationParams
        >>> _.syncBackendWithWallet
        >>> _.beforeBalancing
    )

  contractEnv <- ask
  mbWalletUtxos <- Wallet.getWalletUtxos

  liftAff
    $ withBalancerConstraints
        ownAddresses
        networkId
        protocolParameters
        provider
        logLevel
        customLogger
        constraintsBuilder
    $ runExceptT do
        changeAddress <- getChangeAddress

        mbSrcAddrs <- asksConstraints Constraints._srcAddresses

        changeDatum' <- asksConstraints Constraints._changeDatum

        let
          getWalletCollateralAff = runContractInEnv contractEnv
            getWalletCollateral
          isCip30WalletAff = runContractInEnv contractEnv isCip30Wallet
          syncBackendWithWalletAff = runContractInEnv contractEnv
            syncBackendWithWallet

        (utxos :: UtxoMap) <- do
          case mbSrcAddrs of
            -- Use wallet UTxOs.
            Nothing -> do
              when syncBeforeBalancing $ do
                logWithLevel Info $
                  "balanceTxWithConstraints: syncBackendWithWallet"
                liftAff syncBackendWithWalletAff
              logWithLevel Info $
                "balanceTxWithConstraints: Wallet.getWalletUtxos"
              except $ note CouldNotGetUtxos mbWalletUtxos
            -- Use UTxOs from source addresses
            Just srcAddrs -> do
              -- Even though some of the addresses may be controlled by the wallet,
              -- we can't query the wallet for available UTxOs, because there's no
              -- way to tell it to return UTxOs only from specific subset of the
              -- addresses controlled by a CIP-30 wallet.
              -- `utxosAt` calls are expensive when there are a lot of addresses to
              -- check.
              mbUtxosArr <-
                liftAff $ parTraverse (provider.utxosAt >>> map hush)
                  srcAddrs

              utxosArr <-
                traverse ((except <<< note CouldNotGetUtxos)) mbUtxosArr

              -- merge all utxos into one map
              pure $ foldr Map.union Map.empty utxosArr

        unbalancedCollTx <- transactionWithNetworkId >>=
          if Array.null (transaction ^. _witnessSet <<< _redeemers)
          -- Don't set collateral if tx doesn't contain phase-2 scripts:
          then pure
          else setTransactionCollateral getWalletCollateralAff
            changeAddress
        let
          allUtxos :: UtxoMap
          allUtxos =
            -- Combine utxos at the user address and those from any scripts
            -- involved with the contract in the unbalanced transaction:
            utxos `Map.union` extraUtxos

        availableUtxos <- liftAff $ filterLockedUtxosAff allUtxos

        logWithLevelAndTags Info (pprintUtxoMap allUtxos)
          "balanceTxWithConstraints: all UTxOs"
        logWithLevelAndTags Info (pprintUtxoMap availableUtxos)
          "balanceTxWithConstraints: available UTxOs"

        selectionStrategy <- asksConstraints Constraints._selectionStrategy

        pparams <- asks _.protocolParameters

        -- Balance and finalize the transaction:
        runBalancer
          isCip30WalletAff
          getWalletCollateralAff
          { strategy: selectionStrategy
          , transaction: unbalancedCollTx
          , changeAddress
          , changeDatum: changeDatum'
          , allUtxos
          , utxos: availableUtxos
          , miscFee: getCertsBalance transaction pparams + getProposalsBalance
              transaction
          }
  where
  getChangeAddress :: BalanceTxM Address
  getChangeAddress = maybe (liftAff getChangeAddressAff) pure
    =<< asksConstraints Constraints._changeAddress

  transactionWithNetworkId :: BalanceTxM Transaction
  transactionWithNetworkId = do
    networkId <- maybe askNetworkId pure
      (transaction ^. _body <<< _networkId)
    pure (transaction # _body <<< _networkId ?~ networkId)

