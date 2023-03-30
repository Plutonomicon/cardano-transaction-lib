module Ctl.Internal.BalanceTx.Constraints
  ( BalancerConstraints(BalancerConstraints)
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxosWithOutRefs
  , mustNotSpendUtxoWithOutRef
  , mustSendChangeToAddress
  , mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseUtxosAtAddress
  , mustUseUtxosAtAddresses
  , _additionalUtxos
  , _changeAddress
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _selectionStrategy
  , _srcAddresses
  ) where

import Prelude

import Control.Alt ((<|>))
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyOptimal)
  )
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  )
import Ctl.Internal.Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap) as Plutus
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (appendOver, set, setJust)
import Data.Map (empty, union) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Set (Set)
import Data.Set (singleton, union) as Set
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

newtype BalancerConstraints = BalancerConstraints
  { additionalUtxos :: Plutus.UtxoMap
  , maxChangeOutputTokenQuantity :: Maybe BigInt
  , nonSpendableInputs :: Set TransactionInput
  , srcAddresses :: Maybe (Array Address)
  , changeAddress :: Maybe Address
  , selectionStrategy :: SelectionStrategy
  }

derive instance Newtype BalancerConstraints _
derive instance Generic BalancerConstraints _

derive newtype instance Eq BalancerConstraints

instance Show BalancerConstraints where
  show = genericShow

instance Semigroup BalancerConstraints where
  append (BalancerConstraints a) (BalancerConstraints b) = BalancerConstraints
    { additionalUtxos: Map.union a.additionalUtxos b.additionalUtxos
    , maxChangeOutputTokenQuantity:
        case a.maxChangeOutputTokenQuantity, b.maxChangeOutputTokenQuantity of
          Just ta, Just tb -> Just $ min ta tb
          mta, mtb -> mta <|> mtb
    , nonSpendableInputs: Set.union a.nonSpendableInputs b.nonSpendableInputs
    , srcAddresses: a.srcAddresses <> b.srcAddresses
    , changeAddress: b.changeAddress <|> a.changeAddress -- right-skewed
    , selectionStrategy: b.selectionStrategy
    }

instance Monoid BalancerConstraints where
  mempty = wrap
    { additionalUtxos: Map.empty
    , maxChangeOutputTokenQuantity: Nothing
    , nonSpendableInputs: mempty
    , srcAddresses: Nothing
    , changeAddress: Nothing
    , selectionStrategy: SelectionStrategyOptimal
    }

_additionalUtxos :: Lens' BalancerConstraints Plutus.UtxoMap
_additionalUtxos = _Newtype <<< prop (Proxy :: Proxy "additionalUtxos")

_maxChangeOutputTokenQuantity :: Lens' BalancerConstraints (Maybe BigInt)
_maxChangeOutputTokenQuantity =
  _Newtype <<< prop (Proxy :: Proxy "maxChangeOutputTokenQuantity")

_nonSpendableInputs :: Lens' BalancerConstraints (Set TransactionInput)
_nonSpendableInputs = _Newtype <<< prop (Proxy :: Proxy "nonSpendableInputs")

_srcAddresses :: Lens' BalancerConstraints (Maybe (Array Address))
_srcAddresses = _Newtype <<< prop (Proxy :: Proxy "srcAddresses")

_changeAddress :: Lens' BalancerConstraints (Maybe Address)
_changeAddress = _Newtype <<< prop (Proxy :: Proxy "changeAddress")

_selectionStrategy :: Lens' BalancerConstraints SelectionStrategy
_selectionStrategy = _Newtype <<< prop (Proxy :: Proxy "selectionStrategy")

-- | Tells the balancer to send all generated change to a given address.
-- | If this constraint is not set, then the default change address owned by
-- | the wallet is used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustSendChangeToAddress
  :: Plutus.AddressWithNetworkTag -> BalancerConstraints
mustSendChangeToAddress addr =
  mempty # setJust _changeAddress (fromPlutusAddressWithNetworkTag addr)

-- | Tells the balancer to use UTxO's at given addresses.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddresses
  :: NetworkId -> Array Plutus.Address -> BalancerConstraints
mustUseUtxosAtAddresses networkId address =
  mempty # setJust _srcAddresses (fromPlutusAddress networkId <$> address)

-- | Tells the balancer to use UTxO's at a given address.
-- | If this constraint is not set, then the default addresses owned by the
-- | wallet are used.
-- |
-- | NOTE: Setting `mustUseUtxosAtAddresses` or `mustUseUtxosAtAddress`
-- | does NOT have any effect on which address will be used as a change address.
mustUseUtxosAtAddress
  :: Plutus.AddressWithNetworkTag -> BalancerConstraints
mustUseUtxosAtAddress (Plutus.AddressWithNetworkTag { address, networkId }) =
  mustUseUtxosAtAddresses networkId (Array.singleton address)

-- | Tells the balancer to split change outputs and equipartition change `Value`
-- | between them if the total change `Value` contains token quantities
-- | exceeding the specified upper bound.
-- | (See `Cardano.Types.Value.equipartitionValueWithTokenQuantityUpperBound`)
mustGenChangeOutsWithMaxTokenQuantity :: BigInt -> BalancerConstraints
mustGenChangeOutsWithMaxTokenQuantity q =
  mempty # setJust _maxChangeOutputTokenQuantity (max one q)

-- | Tells the balancer not to spend UTxO's with the specified output references.
mustNotSpendUtxosWithOutRefs
  :: Set TransactionInput -> BalancerConstraints
mustNotSpendUtxosWithOutRefs utxos = mempty # appendOver _nonSpendableInputs
  utxos

-- | Tells the balancer not to spend a UTxO with the specified output reference.
mustNotSpendUtxoWithOutRef :: TransactionInput -> BalancerConstraints
mustNotSpendUtxoWithOutRef = mustNotSpendUtxosWithOutRefs <<< Set.singleton

-- | Tells the balancer to use the provided UTxO set when evaluating script
-- | execution units (sets `additionalUtxoSet` of Ogmios `EvaluateTx`).
-- | Note that you need to use `unspentOutputs` lookup to make these UTxO's
-- | spendable by the transaction (see `Examples.TxChaining` for reference).
mustUseAdditionalUtxos :: Plutus.UtxoMap -> BalancerConstraints
mustUseAdditionalUtxos utxos = mempty # set _additionalUtxos utxos

-- | Tells the balancer to use the given strategy for coin selection.
mustUseCoinSelectionStrategy :: SelectionStrategy -> BalancerConstraints
mustUseCoinSelectionStrategy s = mempty # set _selectionStrategy s
