module Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraintsBuilder
  , BalancerConstraints(BalancerConstraints)
  , BalancerConfig(BalancerConfig)
  , UtxoPredicate
  , buildBalancerConfig
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxosWhere
  , mustNotSpendUtxosWithOutRefs
  , mustNotSpendUtxoWithOutRef
  , mustSendChangeToAddress
  , mustSendChangeWithDatum
  , mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddress
  , mustUseUtxosAtAddresses
  , _additionalUtxos
  , _collateralUtxos
  , _changeAddress
  , _changeDatum
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _nonSpendableInputsPredicates
  , _selectionStrategy
  , _srcAddresses
  ) where

import Prelude

import Cardano.Types (Address, TransactionInput, TransactionOutput, UtxoMap)
import Cardano.Types.OutputDatum (OutputDatum)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyOptimal)
  )
import Data.Array (singleton) as Array
import Data.Function (applyFlipped)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (appendOver, set, setJust)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Set (Set)
import Data.Set (singleton) as Set
import JS.BigInt (BigInt)
import Type.Proxy (Proxy(Proxy))

newtype BalancerConfig = BalancerConfig
  { additionalUtxos :: UtxoMap
  , collateralUtxos :: Maybe UtxoMap
  , maxChangeOutputTokenQuantity :: Maybe BigInt
  , nonSpendableInputs :: Set TransactionInput
  , nonSpendableInputsPredicates :: Array UtxoPredicate
  , srcAddresses :: Maybe (Array Address)
  , changeAddress :: Maybe Address
  , changeDatum :: Maybe OutputDatum
  , selectionStrategy :: SelectionStrategy
  }

derive instance Newtype BalancerConfig _

type UtxoPredicate = TransactionInput -> TransactionOutput -> Boolean

_additionalUtxos :: Lens' BalancerConfig UtxoMap
_additionalUtxos = _Newtype <<< prop (Proxy :: Proxy "additionalUtxos")

_collateralUtxos :: Lens' BalancerConfig (Maybe UtxoMap)
_collateralUtxos = _Newtype <<< prop (Proxy :: Proxy "collateralUtxos")

_maxChangeOutputTokenQuantity :: Lens' BalancerConfig (Maybe BigInt)
_maxChangeOutputTokenQuantity =
  _Newtype <<< prop (Proxy :: Proxy "maxChangeOutputTokenQuantity")

_nonSpendableInputs :: Lens' BalancerConfig (Set TransactionInput)
_nonSpendableInputs = _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputs")

_nonSpendableInputsPredicates :: Lens' BalancerConfig (Array UtxoPredicate)
_nonSpendableInputsPredicates =
  _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputsPredicates")

_srcAddresses :: Lens' BalancerConfig (Maybe (Array Address))
_srcAddresses = _Newtype <<< prop (Proxy :: Proxy "srcAddresses")

_changeAddress :: Lens' BalancerConfig (Maybe Address)
_changeAddress = _Newtype <<< prop (Proxy :: Proxy "changeAddress")

_changeDatum :: Lens' BalancerConfig (Maybe OutputDatum)
_changeDatum = _Newtype <<< prop (Proxy :: Proxy "changeDatum")

_selectionStrategy :: Lens' BalancerConfig SelectionStrategy
_selectionStrategy = _Newtype <<< prop (Proxy :: Proxy "selectionStrategy")

type BalanceTxConstraintsBuilder = BalancerConstraints

newtype BalancerConstraints =
  BalancerConstraints (BalancerConfig -> BalancerConfig)

derive instance Newtype BalancerConstraints _

instance Semigroup BalancerConstraints where
  append = over2 BalancerConstraints (>>>)

instance Monoid BalancerConstraints where
  mempty = wrap identity

buildBalancerConfig :: BalancerConstraints -> BalancerConfig
buildBalancerConfig = applyFlipped defaultConstraints <<< unwrap
  where
  defaultConstraints :: BalancerConfig
  defaultConstraints = wrap
    { additionalUtxos: Map.empty
    , collateralUtxos: Nothing
    , maxChangeOutputTokenQuantity: Nothing
    , nonSpendableInputs: mempty
    , nonSpendableInputsPredicates: mempty
    , srcAddresses: Nothing
    , changeDatum: Nothing
    , changeAddress: Nothing
    , selectionStrategy: SelectionStrategyOptimal
    }

-- | Tells the balancer to send all generated change to a given address.
-- | If this constraint is not set, then the default change address owned by
-- | the wallet is used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustSendChangeToAddress
  :: Address -> BalancerConstraints
mustSendChangeToAddress =
  wrap <<< setJust _changeAddress

-- | Tells the balancer to include the datum in each change UTxO. Useful when
-- | balancing a transactions for script owned UTxOs.
mustSendChangeWithDatum :: OutputDatum -> BalancerConstraints
mustSendChangeWithDatum =
  wrap <<< setJust _changeDatum

-- | Tells the balancer to use UTxO's at given addresses.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddresses
  :: Array Address -> BalancerConstraints
mustUseUtxosAtAddresses =
  wrap <<< setJust _srcAddresses

-- | Tells the balancer to use UTxO's at a given address.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddress
  :: Address -> BalancerConstraints
mustUseUtxosAtAddress address =
  mustUseUtxosAtAddresses (Array.singleton address)

-- | Tells the balancer to split change outputs and equipartition change `Value`
-- | between them if the total change `Value` contains token quantities
-- | exceeding the specified upper bound.
-- | (See `Cardano.Types.Value.equipartitionValueWithTokenQuantityUpperBound`)
mustGenChangeOutsWithMaxTokenQuantity :: BigInt -> BalancerConstraints
mustGenChangeOutsWithMaxTokenQuantity =
  wrap <<< setJust _maxChangeOutputTokenQuantity <<< max one

-- | Tells the balancer not to spend UTxO's with the specified output references.
mustNotSpendUtxosWithOutRefs
  :: Set TransactionInput -> BalancerConstraints
mustNotSpendUtxosWithOutRefs = wrap <<< appendOver _nonSpendableInputs

-- | Tells the balancer not to spend a UTxO with the specified output reference.
mustNotSpendUtxoWithOutRef :: TransactionInput -> BalancerConstraints
mustNotSpendUtxoWithOutRef = mustNotSpendUtxosWithOutRefs <<< Set.singleton

-- | Tells the balancer not to spend UTxO's based on the given predicate.
-- | Note that `mustNotSpendUtxosWhere` constraints are stacked when specified
-- | multiple times, and utxos are tested against each predicate. The order of
-- | specifying multiple `mustNotSpendUtxosWhere` constraints does NOT affect
-- | the resulting set.
mustNotSpendUtxosWhere :: UtxoPredicate -> BalanceTxConstraintsBuilder
mustNotSpendUtxosWhere =
  wrap
    <<< appendOver _nonSpendableInputsPredicates
    <<< Array.singleton

-- | Tells the balancer to use the provided UTxO set when evaluating script
-- | execution units (sets `additionalUtxoSet` of Ogmios `EvaluateTx`).
-- | Note that you need to use `unspentOutputs` lookup to make these UTxO's
-- | spendable by the transaction (see `Examples.TxChaining` for reference).
mustUseAdditionalUtxos :: UtxoMap -> BalancerConstraints
mustUseAdditionalUtxos = wrap <<< set _additionalUtxos

-- | Tells the balancer to select from the provided UTxO set when choosing
-- | collateral UTxOs, instead of UTxOs provided by the browser wallet.
mustUseCollateralUtxos :: UtxoMap -> BalancerConstraints
mustUseCollateralUtxos = wrap <<< set _collateralUtxos <<< Just

-- | Tells the balancer to use the given strategy for coin selection.
mustUseCoinSelectionStrategy :: SelectionStrategy -> BalancerConstraints
mustUseCoinSelectionStrategy = wrap <<< set _selectionStrategy
