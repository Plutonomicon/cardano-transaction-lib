-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( utxosAt
  , module Transaction
  ) where

import Prelude
import Contract.Monad (Contract, wrapContract)
import Data.Maybe (Maybe)
import QueryM.Utxos (utxosAt) as Utxos
import Serialization.Address (Address)
-- Can potentially remove, perhaps we move utxo related all to Contract.Address
-- and/or Contract.Transaction. Perhaps it's best to not expose JsonWsp.
import Types.Transaction (Utxo, UtxoM(UtxoM)) as Transaction

-- | This module defines query functionality via Ogmios to get utxos.

-- | Gets utxos at an (internal) `Address` in terms of (internal) `Transaction.Types`.
-- | Results may vary depending on `Wallet` type. See `QueryM` for more details
-- | on wallet variance.
utxosAt
  :: forall (r :: Row Type). Address -> Contract r (Maybe Transaction.UtxoM)
utxosAt = wrapContract <<< Utxos.utxosAt
