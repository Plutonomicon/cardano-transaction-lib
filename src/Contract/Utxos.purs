-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( getUtxo
  , utxosAt
  ) where

import Prelude

import Cardano.Types (Address, TransactionOutput, UtxoMap)
import Cardano.Types.TransactionInput (TransactionInput)
import Contract.Log (logWarn')
import Contract.Monad (Contract, liftedE)
import Ctl.Internal.BalanceTx.Sync (getControlledAddresses, isCip30Wallet)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Data.Maybe (Maybe)
import Data.Set (member) as Set
import Effect.Aff.Class (liftAff)

-- | Queries for UTxOs at the given `Address`.
-- |
-- | **Note**: calling `utxosAt` on an address controlled by the light
-- | wallet may result in hard-to-debug problems with wallet interactions.
-- | The developers should not assume that all UTxOs that are available on
-- | wallet addresses are actually spendable. See the docs for UTxO locking
-- | in `doc/query-layers.md`. Using `getWalletUtxos` is a way to avoid the
-- | potential problems. This function will raise a warning in the logs if
-- | wallet address is used.
utxosAt
  :: Address
  -> Contract UtxoMap
utxosAt address = do
  queryHandle <- getQueryHandle
  whenM isCip30Wallet do
    walletAddresses <- getControlledAddresses
    when (address `Set.member` walletAddresses) do
      logWarn' $
        "utxosAt: you are calling `utxosAt` on an address controlled by the"
          <> " wallet. This may result in hard-to-debug problems with wallet "
          <> "interactions. The developers should not assume that all UTxOs "
          <> "that are available on wallet addresses are actually spendable. "
          <> "See the docs for UTxO locking in `doc/query-layers.md`. Using "
          <> "`getWalletUtxos` is a way to avoid the potential problems."
  liftedE $ liftAff $ queryHandle.utxosAt address

-- | Queries for an utxo given a transaction input.
-- | Returns `Nothing` if the output has already been spent.
getUtxo
  :: TransactionInput
  -> Contract (Maybe TransactionOutput)
getUtxo oref = do
  queryHandle <- getQueryHandle
  liftedE $ liftAff $ queryHandle.getUtxoByOref oref
