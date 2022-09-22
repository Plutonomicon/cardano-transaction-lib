module BalanceTx.Constraints
  ( BalanceTxConstraints(BalanceTxConstraints)
  , BalanceTxConstraintsBuilder(BalanceTxConstraintsBuilder)
  , buildBalanceTxConstraints
  , mustBalanceTxWithAddress
  , mustBalanceTxWithAddress'
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustUseAdditionalUtxos
  , _additionalUtxos
  , _maxChangeOutputTokenQuantity
  , _ownAddress
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Function (applyFlipped)
import Data.Lens (Lens')
import Data.Lens.Setter (set, setJust)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Plutus.Conversion (fromPlutusAddress)
import Plutus.Types.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import Plutus.Types.Transaction (UtxoMap) as Plutus
import Serialization.Address (Address, NetworkId)
import Type.Proxy (Proxy(Proxy))

newtype BalanceTxConstraints = BalanceTxConstraints
  { additionalUtxos :: Plutus.UtxoMap
  , maxChangeOutputTokenQuantity :: Maybe BigInt
  , ownAddress :: Maybe Address
  }

derive instance Newtype BalanceTxConstraints _

_additionalUtxos :: Lens' BalanceTxConstraints Plutus.UtxoMap
_additionalUtxos = _Newtype <<< prop (Proxy :: Proxy "additionalUtxos")

_maxChangeOutputTokenQuantity :: Lens' BalanceTxConstraints (Maybe BigInt)
_maxChangeOutputTokenQuantity =
  _Newtype <<< prop (Proxy :: Proxy "maxChangeOutputTokenQuantity")

_ownAddress :: Lens' BalanceTxConstraints (Maybe Address)
_ownAddress = _Newtype <<< prop (Proxy :: Proxy "ownAddress")

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
    , ownAddress: Nothing
    }

mustBalanceTxWithAddress
  :: NetworkId -> Plutus.Address -> BalanceTxConstraintsBuilder
mustBalanceTxWithAddress networkId =
  wrap <<< setJust _ownAddress <<< fromPlutusAddress networkId

mustBalanceTxWithAddress'
  :: Plutus.AddressWithNetworkTag -> BalanceTxConstraintsBuilder
mustBalanceTxWithAddress'
  (Plutus.AddressWithNetworkTag { address, networkId }) =
  mustBalanceTxWithAddress networkId address

mustGenChangeOutsWithMaxTokenQuantity :: BigInt -> BalanceTxConstraintsBuilder
mustGenChangeOutsWithMaxTokenQuantity =
  wrap <<< setJust _maxChangeOutputTokenQuantity <<< max one

mustUseAdditionalUtxos :: Plutus.UtxoMap -> BalanceTxConstraintsBuilder
mustUseAdditionalUtxos = wrap <<< set _additionalUtxos

