module Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraints(BalanceTxConstraints)
  , BalanceTxConstraintsBuilder(BalanceTxConstraintsBuilder)
  , UtxoPredicate
  , buildBalanceTxConstraints
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxosWhere
  , mustNotSpendUtxosWithOutRefs
  , mustNotSpendUtxoWithOutRef
  , mustSendChangeToAddress
  , mustSendChangeWithDatum
  , mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseUtxosAtAddress
  , mustUseUtxosAtAddresses
  , _additionalUtxos
  , _changeAddress
  , _changeDatum
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _nonSpendableInputsPredicates
  , _selectionStrategy
  , _srcAddresses
  ) where

import Prelude

import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyOptimal)
  )
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusTxOutputWithRefScript
  )
import Ctl.Internal.Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutputWithRefScript
  , UtxoMap
  ) as Plutus
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Types.OutputDatum (OutputDatum)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.Function (applyFlipped)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (appendOver, set, setJust)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Set (Set)
import Data.Set (singleton) as Set
import Type.Proxy (Proxy(Proxy))

newtype BalanceTxConstraints = BalanceTxConstraints
  { additionalUtxos :: Plutus.UtxoMap
  , maxChangeOutputTokenQuantity :: Maybe BigInt
  , nonSpendableInputs :: Set TransactionInput
  , nonSpendableInputsPredicates :: Array (UtxoPredicate TransactionOutput)
  , srcAddresses :: Maybe (Array Address)
  , changeAddress :: Maybe Address
  , changeDatum :: Maybe OutputDatum
  , selectionStrategy :: SelectionStrategy
  }

derive instance Newtype BalanceTxConstraints _

type UtxoPredicate (output :: Type) = TransactionInput -> output -> Boolean

_additionalUtxos :: Lens' BalanceTxConstraints Plutus.UtxoMap
_additionalUtxos = _Newtype <<< prop (Proxy :: Proxy "additionalUtxos")

_maxChangeOutputTokenQuantity :: Lens' BalanceTxConstraints (Maybe BigInt)
_maxChangeOutputTokenQuantity =
  _Newtype <<< prop (Proxy :: Proxy "maxChangeOutputTokenQuantity")

_nonSpendableInputs :: Lens' BalanceTxConstraints (Set TransactionInput)
_nonSpendableInputs = _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputs")

_nonSpendableInputsPredicates
  :: Lens' BalanceTxConstraints (Array (UtxoPredicate TransactionOutput))
_nonSpendableInputsPredicates =
  _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputsPredicates")

_srcAddresses :: Lens' BalanceTxConstraints (Maybe (Array Address))
_srcAddresses = _Newtype <<< prop (Proxy :: Proxy "srcAddresses")

_changeAddress :: Lens' BalanceTxConstraints (Maybe Address)
_changeAddress = _Newtype <<< prop (Proxy :: Proxy "changeAddress")

_changeDatum :: Lens' BalanceTxConstraints (Maybe OutputDatum)
_changeDatum = _Newtype <<< prop (Proxy :: Proxy "changeDatum")

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
    { additionalUtxos: Map.empty
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
  :: Plutus.AddressWithNetworkTag -> BalanceTxConstraintsBuilder
mustSendChangeToAddress =
  wrap <<< setJust _changeAddress <<< fromPlutusAddressWithNetworkTag

-- | Tells the balancer to include the datum in each change UTxO. Useful when
-- | balancing a transactions for script owned UTxOs.
mustSendChangeWithDatum :: OutputDatum -> BalanceTxConstraintsBuilder
mustSendChangeWithDatum =
  wrap <<< setJust _changeDatum

-- | Tells the balancer to use UTxO's at given addresses.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddresses
  :: NetworkId -> Array Plutus.Address -> BalanceTxConstraintsBuilder
mustUseUtxosAtAddresses networkId =
  wrap <<< setJust _srcAddresses <<< map (fromPlutusAddress networkId)

-- | Tells the balancer to use UTxO's at a given address.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddress
  :: Plutus.AddressWithNetworkTag -> BalanceTxConstraintsBuilder
mustUseUtxosAtAddress (Plutus.AddressWithNetworkTag { address, networkId }) =
  mustUseUtxosAtAddresses networkId (Array.singleton address)

-- | Tells the balancer to split change outputs and equipartition change `Value`
-- | between them if the total change `Value` contains token quantities
-- | exceeding the specified upper bound.
-- | (See `Cardano.Types.Value.equipartitionValueWithTokenQuantityUpperBound`)
mustGenChangeOutsWithMaxTokenQuantity :: BigInt -> BalanceTxConstraintsBuilder
mustGenChangeOutsWithMaxTokenQuantity =
  wrap <<< setJust _maxChangeOutputTokenQuantity <<< max one

-- | Tells the balancer not to spend UTxO's with the specified output references.
mustNotSpendUtxosWithOutRefs
  :: Set TransactionInput -> BalanceTxConstraintsBuilder
mustNotSpendUtxosWithOutRefs = wrap <<< appendOver _nonSpendableInputs

-- | Tells the balancer not to spend a UTxO with the specified output reference.
mustNotSpendUtxoWithOutRef :: TransactionInput -> BalanceTxConstraintsBuilder
mustNotSpendUtxoWithOutRef = mustNotSpendUtxosWithOutRefs <<< Set.singleton

-- | Tells the balancer not to spend UTxO's based on the given predicate.
-- | Note that `mustNotSpendUtxosWhere` constraints are stacked when specified
-- | multiple times, and utxos are tested against each predicate. The order of
-- | specifying multiple `mustNotSpendUtxosWhere` constraints does NOT affect
-- | the resulting set.
mustNotSpendUtxosWhere
  :: UtxoPredicate Plutus.TransactionOutputWithRefScript
  -> BalanceTxConstraintsBuilder
mustNotSpendUtxosWhere p =
  wrap $ appendOver _nonSpendableInputsPredicates
    ( Array.singleton \oref out ->
        fromMaybe false $ p oref <$> toPlutusTxOutputWithRefScript out
    )

-- | Tells the balancer to use the provided UTxO set when evaluating script
-- | execution units (sets `additionalUtxoSet` of Ogmios `EvaluateTx`).
-- | Note that you need to use `unspentOutputs` lookup to make these UTxO's
-- | spendable by the transaction (see `Examples.TxChaining` for reference).
mustUseAdditionalUtxos :: Plutus.UtxoMap -> BalanceTxConstraintsBuilder
mustUseAdditionalUtxos = wrap <<< set _additionalUtxos

-- | Tells the balancer to use the given strategy for coin selection.
mustUseCoinSelectionStrategy :: SelectionStrategy -> BalanceTxConstraintsBuilder
mustUseCoinSelectionStrategy = wrap <<< set _selectionStrategy
