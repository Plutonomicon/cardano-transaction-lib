module Ctl.Internal.BalanceTx.UnattachedTx
  ( UnattachedTx
  , UnindexedTx
  , IndexedTx
  , EvaluatedTx
  , PrebalancedTx(PrebalancedTx)
  , indexTx
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( IndexedRedeemer
  , UnindexedRedeemer
  , attachRedeemers
  , indexRedeemers
  , mkRedeemersContext
  )
import Ctl.Internal.Cardano.Types.Transaction (Redeemer, Transaction)
import Ctl.Internal.Types.Datum (Datum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

type UnattachedTx row =
  (Record (transaction :: Transaction, datums :: Array Datum | row))

type UnindexedTx = UnattachedTx (redeemers :: Array UnindexedRedeemer)
type IndexedTx = UnattachedTx (redeemers :: Array IndexedRedeemer)
type EvaluatedTx = UnattachedTx (redeemers :: Array Redeemer)

newtype PrebalancedTx = PrebalancedTx UnindexedTx

derive instance Generic PrebalancedTx _
derive instance Newtype PrebalancedTx _
derive newtype instance Eq PrebalancedTx
derive newtype instance EncodeAeson PrebalancedTx

indexTx :: UnindexedTx -> Maybe IndexedTx
indexTx { transaction, datums, redeemers } = do
  redeemers' <- indexRedeemers (mkRedeemersContext transaction) redeemers
  pure
    { transaction: attachRedeemers redeemers' transaction
    , datums
    , redeemers: redeemers'
    }

instance Show PrebalancedTx where
  show = genericShow
