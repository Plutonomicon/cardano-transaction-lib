module Contract.Transaction
  ( balanceTx
  , balanceTxM
  , signTransaction
  , submitTransaction
  ) where

import Prelude
import BalanceTx (BalanceTxError, balanceTx) as BalanceTx
import Contract.Monad (Contract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import QueryM (signTransaction, submitTransaction) as QueryM
import Types.Transaction (Transaction, TransactionHash)
import Types.UnbalancedTransaction (UnbalancedTx)

-- | This module defines transaction-related requests. Currently signing and
-- | submission is done with Nami.

-- | Signs a `Transaction` with potential failure.
signTransaction :: Transaction -> Contract (Maybe Transaction)
signTransaction = wrap <<< QueryM.signTransaction

-- | Submits a `Transaction` with potential failure.
submitTransaction :: Transaction -> Contract (Maybe TransactionHash)
submitTransaction = wrap <<< QueryM.submitTransaction

-- | Attempts to balance an `UnbalancedTx`.
balanceTx
  :: UnbalancedTx -> Contract (Either BalanceTx.BalanceTxError Transaction)
balanceTx = wrap <<< BalanceTx.balanceTx

-- | Attempts to balance an `UnbalancedTx` hushing the error.
balanceTxM :: UnbalancedTx -> Contract (Maybe Transaction)
balanceTxM = map hush <<< wrap <<< BalanceTx.balanceTx