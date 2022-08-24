module BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  ) where

import Prelude

import BalanceTx.Error (BalanceTxError)
import Cardano.Types.Transaction (Transaction)
import Control.Monad.Except.Trans (ExceptT)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import QueryM (QueryMExtended)
import Types.ScriptLookups (UnattachedUnbalancedTx)

type BalanceTxM (a :: Type) = ExceptT BalanceTxError (QueryMExtended ()) a

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
