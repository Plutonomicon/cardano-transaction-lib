module Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction(FinalizedTransaction)
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCostModelsForLanguages
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.Cardano.Types.Transaction (Costmdls(Costmdls), Transaction)
import Ctl.Internal.QueryM (QueryMExtended)
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Types.Scripts (Language)
import Data.Generic.Rep (class Generic)
import Data.Map (filterKeys) as Map
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Set (member) as Set
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)

type BalanceTxM (a :: Type) = ExceptT BalanceTxError (QueryMExtended () Aff) a

askCostModelsForLanguages :: Set Language -> BalanceTxM Costmdls
askCostModelsForLanguages languages =
  asks (_.costModels <<< unwrap <<< _.pparams <<< _.runtime)
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
