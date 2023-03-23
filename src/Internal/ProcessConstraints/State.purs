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
  ) where

import Prelude hiding (join)

import Control.Monad.State.Trans (StateT)
import Ctl.Internal.BalanceTx.RedeemerIndex (UnindexedRedeemer)
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls
  , Transaction
  , TransactionOutput
  )
import Ctl.Internal.Cardano.Types.Value (Value, negation, split)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutputWithRefScript) as Plutus
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple.Nested ((/\))

-- A `StateT` ontop of `QueryM` ~ ReaderT QueryConfig Aff`.
-- The state is `ConstraintProcessingState`, which keeps track of the unbalanced
-- transaction etc and additionally holds a `ConstraintsConfig` containing the
-- scriptlookups and a `defaultSlotConfig`.
-- We write `ReaderT QueryConfig Aff` below since type synonyms need to be fully
-- applied.
type ConstraintsM (a :: Type) (b :: Type) =
  StateT (ConstraintProcessingState a) Contract b

-- This is the state for essentially creating an unbalanced transaction.
type ConstraintProcessingState (a :: Type) =
  { transaction :: Transaction
  -- ^ The unbalanced transaction that we're building
  , usedUtxos :: Map TransactionInput TransactionOutput
  -- ^ All UTxOs that are used in the Tx
  , valueSpentBalancesInputs :: ValueSpentBalances
  -- ^ Balance of the values given and required for the transaction's inputs
  , valueSpentBalancesOutputs :: ValueSpentBalances
  -- ^ Balance of the values produced and required for the transaction's outputs
  , datums :: Array Datum
  -- ^ Ordered accumulation of datums we can use to `setScriptDataHash`
  , redeemers :: Array UnindexedRedeemer
  -- ^ Unindexed redeemers that will be attached to the Tx later, on balancing
  -- stage.
  , lookups :: ScriptLookups a
  -- ^ ScriptLookups for resolving constraints. Should be treated as an immutable
  -- value despite living inside the processing state
  -- TODO: remove: https://github.com/Plutonomicon/cardano-transaction-lib/issues/843
  , refScriptsUtxoMap ::
      Map TransactionInput Plutus.TransactionOutputWithRefScript
  , costModels :: Costmdls
  }

_cpsTransaction
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) Transaction
_cpsTransaction = prop (SProxy :: SProxy "transaction")

_cpsUsedUtxos
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a)
       (Map TransactionInput TransactionOutput)
_cpsUsedUtxos = prop (SProxy :: SProxy "usedUtxos")

_valueSpentBalancesInputs
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) ValueSpentBalances
_valueSpentBalancesInputs = prop (SProxy :: SProxy "valueSpentBalancesInputs")

_valueSpentBalancesOutputs
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) ValueSpentBalances
_valueSpentBalancesOutputs = prop (SProxy :: SProxy "valueSpentBalancesOutputs")

_datums
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) (Array Datum)
_datums = prop (SProxy :: SProxy "datums")

_costModels
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) Costmdls
_costModels = prop (SProxy :: SProxy "costModels")

_redeemers
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a)
       (Array UnindexedRedeemer)
_redeemers = prop (SProxy :: SProxy "redeemers")

_lookups
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) (ScriptLookups a)
_lookups = prop (SProxy :: SProxy "lookups")

_refScriptsUtxoMap
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a)
       (Map TransactionInput Plutus.TransactionOutputWithRefScript)
_refScriptsUtxoMap = prop (SProxy :: SProxy "refScriptsUtxoMap")

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

missingValueSpent :: ValueSpentBalances -> Value
missingValueSpent (ValueSpentBalances { required, provided }) =
  let
    difference = required <> negation provided
    _ /\ missing = split difference
  in
    missing

totalMissingValue :: forall (a :: Type). ConstraintProcessingState a -> Value
totalMissingValue { valueSpentBalancesInputs, valueSpentBalancesOutputs } =
  missingValueSpent valueSpentBalancesInputs `join`
    missingValueSpent valueSpentBalancesOutputs

provideValue :: Value -> ValueSpentBalances
provideValue provided = ValueSpentBalances { provided, required: mempty }

requireValue :: Value -> ValueSpentBalances
requireValue required = ValueSpentBalances { required, provided: mempty }
