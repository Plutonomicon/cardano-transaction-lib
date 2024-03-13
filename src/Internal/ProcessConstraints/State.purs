module Ctl.Internal.ProcessConstraints.State
  ( ConstraintsM
  , ConstraintProcessingState
  , _cpsTransaction
  , _cpsUsedUtxos
  , _valueSpentBalancesInputs
  , _valueSpentBalancesOutputs
  , _datums
  , _costModels
  , _redeemers
  , _lookups
  , _refScriptsUtxoMap
  , ValueSpentBalances(ValueSpentBalances)
  , missingValueSpent
  , totalMissingValue
  , provideValue
  , requireValue
  , sumValueSpentBalances
  ) where

import Prelude hiding (join)

import Cardano.Types (CostModel, Language, PlutusData, Transaction, UtxoMap)
import Cardano.Types.Value (Value)
import Cardano.Types.Value as Value
import Control.Monad.State.Trans (StateT)
import Ctl.Internal.BalanceTx.RedeemerIndex (UnindexedRedeemer)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

-- A `StateT` ontop of `QueryM` ~ ReaderT QueryConfig Aff`.
-- The state is `ConstraintProcessingState`, which keeps track of the unbalanced
-- transaction etc and additionally holds a `ConstraintsConfig` containing the
-- scriptlookups and a `defaultSlotConfig`.
-- We write `ReaderT QueryConfig Aff` below since type synonyms need to be fully
-- applied.
type ConstraintsM (a :: Type) =
  StateT ConstraintProcessingState Contract a

-- This is the state for essentially creating an unbalanced transaction.
type ConstraintProcessingState =
  { transaction :: Transaction
  -- ^ The unbalanced transaction that we're building
  , usedUtxos :: UtxoMap
  -- ^ All UTxOs that are used in the Tx
  , valueSpentBalancesInputs :: ValueSpentBalances
  -- ^ Balance of the values given and required for the transaction's inputs
  , valueSpentBalancesOutputs :: ValueSpentBalances
  -- ^ Balance of the values produced and required for the transaction's outputs
  , datums :: Array PlutusData
  -- ^ Ordered accumulation of datums we can use to `setScriptDataHash`
  , redeemers :: Array UnindexedRedeemer
  -- ^ Unindexed redeemers that will be attached to the Tx later, on balancing
  -- stage.
  , lookups :: ScriptLookups
  -- ^ ScriptLookups for resolving constraints. Should be treated as an immutable
  -- value despite living inside the processing state
  -- TODO: remove: https://github.com/Plutonomicon/cardano-transaction-lib/issues/843
  , refScriptsUtxoMap :: UtxoMap
  , costModels :: Map Language CostModel
  }

_cpsTransaction
  :: Lens' ConstraintProcessingState Transaction
_cpsTransaction = prop (Proxy :: Proxy "transaction")

_cpsUsedUtxos
  :: Lens' ConstraintProcessingState UtxoMap
_cpsUsedUtxos = prop (Proxy :: Proxy "usedUtxos")

_valueSpentBalancesInputs
  :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesInputs = prop (Proxy :: Proxy "valueSpentBalancesInputs")

_valueSpentBalancesOutputs
  :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesOutputs = prop (Proxy :: Proxy "valueSpentBalancesOutputs")

_datums
  :: Lens' ConstraintProcessingState (Array PlutusData)
_datums = prop (Proxy :: Proxy "datums")

_costModels
  :: Lens' ConstraintProcessingState (Map Language CostModel)
_costModels = prop (Proxy :: Proxy "costModels")

_redeemers
  :: Lens' ConstraintProcessingState (Array UnindexedRedeemer)
_redeemers = prop (Proxy :: Proxy "redeemers")

_lookups
  :: Lens' ConstraintProcessingState ScriptLookups
_lookups = prop (Proxy :: Proxy "lookups")

_refScriptsUtxoMap
  :: Lens' ConstraintProcessingState UtxoMap
_refScriptsUtxoMap = prop (Proxy :: Proxy "refScriptsUtxoMap")

-- | The balances we track for computing the missing 'Value' (if any)
-- | that needs to be added to the transaction.
-- | See note [Balance of value spent].
newtype ValueSpentBalances = ValueSpentBalances
  { required :: Value
  -- Required value spent by the transaction.
  , provided :: Value
  -- Value provided by an input or output of the transaction.
  }

derive instance Generic ValueSpentBalances _

instance Show ValueSpentBalances where
  show = genericShow

instance Semigroup ValueSpentBalances where
  append (ValueSpentBalances l) (ValueSpentBalances r) = ValueSpentBalances
    { required: l.required `join` r.required -- least upper bound on Value
    , provided: l.provided `join` r.provided
    }

sumValueSpentBalances
  :: ValueSpentBalances -> ValueSpentBalances -> Maybe ValueSpentBalances
sumValueSpentBalances (ValueSpentBalances a) (ValueSpentBalances b) = do
  required <- Value.add a.required b.required
  provided <- Value.add a.provided b.provided
  pure $ ValueSpentBalances { required, provided }

missingValueSpent :: ValueSpentBalances -> Maybe Value
missingValueSpent (ValueSpentBalances { required, provided }) =
  Value.minus required provided

totalMissingValue :: ConstraintProcessingState -> Maybe Value
totalMissingValue { valueSpentBalancesInputs, valueSpentBalancesOutputs } =
  join <$> missingValueSpent valueSpentBalancesInputs <*>
    missingValueSpent valueSpentBalancesOutputs

provideValue :: Value -> ValueSpentBalances
provideValue provided = ValueSpentBalances { provided, required: mempty }

requireValue :: Value -> ValueSpentBalances
requireValue required = ValueSpentBalances { required, provided: mempty }
