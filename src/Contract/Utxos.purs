module Contract.Utxos
  ( utxosAt
  ) where

import Prelude
import Contract.Monad (Contract)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import QueryM (utxosAt) as QueryM
import Serialization.Address (Address)
import Types.Transaction (UtxoM)

-- | This module defines query functionality via Ogmios to get utxos.

-- | Gets utxos at an (internal) `Address` in terms of (internal) `Transaction.Types`.
-- | Results may vary depending on `Wallet` type. See `QueryM` for more details
-- | on wallet variance.
utxosAt :: Address -> Contract (Maybe UtxoM)
utxosAt = wrap <<< QueryM.utxosAt
