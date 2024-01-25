module Ctl.Internal.BalanceTx.UnattachedTx
  ( UnattachedTx
  , UnindexedTx
  , IndexedTx
  , EvaluatedTx
  , indexTx
  , _transaction
  ) where

import Prelude

import Ctl.Internal.BalanceTx.RedeemerIndex
  ( IndexedRedeemer
  , UnindexedRedeemer
  , attachIndexedRedeemers
  , indexRedeemers
  , mkRedeemersContext
  )
import Ctl.Internal.Cardano.Types.Transaction (Redeemer, Transaction)
import Ctl.Internal.Types.Datum (Datum)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(Proxy))

type UnattachedTx redeemer =
  { transaction :: Transaction
  , datums :: Array Datum
  , redeemers :: Array redeemer
  }

-- | A Tx with unindexed redeemers
type UnindexedTx = UnattachedTx UnindexedRedeemer

-- | A Tx with indexed, but not yet evaluated redeemers
type IndexedTx = UnattachedTx IndexedRedeemer

-- | A Tx with fully indexed and evaluated redeemers
type EvaluatedTx = UnattachedTx Redeemer

indexTx :: UnindexedTx -> Either UnindexedRedeemer IndexedTx
indexTx { transaction, datums, redeemers } = do
  redeemers' <- indexRedeemers (mkRedeemersContext transaction) redeemers
  pure
    { transaction: attachIndexedRedeemers redeemers' transaction
    , datums
    , redeemers: redeemers'
    }

_transaction
  :: forall (redeemer :: Type). Lens' (UnattachedTx redeemer) Transaction
_transaction = prop (Proxy :: Proxy "transaction")
