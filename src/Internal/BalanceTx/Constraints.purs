module Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraints(BalanceTxConstraints)
  , BalanceTxConstraintsBuilder(BalanceTxConstraintsBuilder)
  , buildBalanceTxConstraints
  , mustBalanceTxWithAddress
  , mustBalanceTxWithAddresses
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxosWithOutRefs
  , mustNotSpendUtxoWithOutRef
  , mustUseCoinSelectionStrategy
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _ownAddresses
  , _selectionStrategy
  ) where

import Prelude

import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyOptimal)
  )
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Ctl.Internal.Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.Function (applyFlipped)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (appendOver, set, setJust)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Set (Set)
import Data.Set (singleton) as Set
import Type.Proxy (Proxy(Proxy))

newtype BalanceTxConstraints = BalanceTxConstraints
  { maxChangeOutputTokenQuantity :: Maybe BigInt
  , nonSpendableInputs :: Set TransactionInput
  , ownAddresses :: Maybe (Array Address)
  , selectionStrategy :: SelectionStrategy
  }

derive instance Newtype BalanceTxConstraints _

_maxChangeOutputTokenQuantity :: Lens' BalanceTxConstraints (Maybe BigInt)
_maxChangeOutputTokenQuantity =
  _Newtype <<< prop (Proxy :: Proxy "maxChangeOutputTokenQuantity")

_nonSpendableInputs :: Lens' BalanceTxConstraints (Set TransactionInput)
_nonSpendableInputs = _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputs")

_ownAddresses :: Lens' BalanceTxConstraints (Maybe (Array Address))
_ownAddresses = _Newtype <<< prop (Proxy :: Proxy "ownAddresses")

_selectionStrategy :: Lens' BalanceTxConstraints SelectionStrategy
_selectionStrategy = _Newtype <<< prop (Proxy :: Proxy "selectionStrategy")

newtype BalanceTxConstraintsBuilder =
  BalanceTxConstraintsBuilder (BalanceTxConstraints -> BalanceTxConstraints)

derive instance Newtype BalanceTxConstraintsBuilder _

instance Semigroup BalanceTxConstraintsBuilder where
  append = over2 BalanceTxConstraintsBuilder (>>>)

instance Monoid BalanceTxConstraintsBuilder where
  mempty = wrap identity

buildBalanceTxConstraints :: BalanceTxConstraintsBuilder -> BalanceTxConstraints
buildBalanceTxConstraints = applyFlipped defaultConstraints <<< unwrap
  where
  defaultConstraints :: BalanceTxConstraints
  defaultConstraints = wrap
    { maxChangeOutputTokenQuantity: Nothing
    , nonSpendableInputs: mempty
    , ownAddresses: Nothing
    , selectionStrategy: SelectionStrategyOptimal
    }

-- | Tells the balancer to treat the provided addresses like user's own.
mustBalanceTxWithAddresses
  :: NetworkId -> Array Plutus.Address -> BalanceTxConstraintsBuilder
mustBalanceTxWithAddresses networkId =
  wrap <<< setJust _ownAddresses <<< map (fromPlutusAddress networkId)

-- | Tells the balancer to treat the provided address like user's own.
-- | Like `mustBalanceTxWithAddress`, but takes `AddressWithNetworkTag`. 
mustBalanceTxWithAddress
  :: Plutus.AddressWithNetworkTag -> BalanceTxConstraintsBuilder
mustBalanceTxWithAddress
  (Plutus.AddressWithNetworkTag { address, networkId }) =
  mustBalanceTxWithAddresses networkId (Array.singleton address)

-- | Tells the balancer to split change outputs and equipartition change `Value` 
-- | between them if the total change `Value` contains token quantities 
-- | exceeding the specified upper bound.
-- | (See `Cardano.Types.Value.equipartitionValueWithTokenQuantityUpperBound`)
mustGenChangeOutsWithMaxTokenQuantity :: BigInt -> BalanceTxConstraintsBuilder
mustGenChangeOutsWithMaxTokenQuantity =
  wrap <<< setJust _maxChangeOutputTokenQuantity <<< max one

-- | Tells the balancer not to spend utxos with the specified output references.
mustNotSpendUtxosWithOutRefs
  :: Set TransactionInput -> BalanceTxConstraintsBuilder
mustNotSpendUtxosWithOutRefs = wrap <<< appendOver _nonSpendableInputs

-- | Tells the balancer not to spend a utxo with the specified output reference.
mustNotSpendUtxoWithOutRef :: TransactionInput -> BalanceTxConstraintsBuilder
mustNotSpendUtxoWithOutRef = mustNotSpendUtxosWithOutRefs <<< Set.singleton

-- | Tells the balancer to use the given strategy for coin selection.
mustUseCoinSelectionStrategy :: SelectionStrategy -> BalanceTxConstraintsBuilder
mustUseCoinSelectionStrategy = wrap <<< set _selectionStrategy

-- TODO: https://github.com/Plutonomicon/cardano-transaction-lib/pull/1046
-- | Tells the balancer to use the provided utxo set when evaluating script 
-- | execution units (Sets `additionalUtxoSet` of Ogmios `EvaluateTx`).
-- mustUseAdditionalUtxos :: Plutus.UtxoMap -> BalanceTxConstraintsBuilder
-- mustUseAdditionalUtxos = wrap <<< set _additionalUtxos
