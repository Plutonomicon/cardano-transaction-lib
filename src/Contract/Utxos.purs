-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( getUtxo
  , utxosAt
  , utxosAtScriptHash
  , module X
  ) where

import Prelude

import Contract.Log (logWarn')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.Prelude (for)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.BalanceTx.Sync (getControlledAddresses, isCip30Wallet)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddress
  , toPlutusTxOutput
  , toPlutusUtxoMap
  )
import Ctl.Internal.Plutus.Types.Address (class PlutusAddress, getAddress)
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap) as X
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Types.Transaction (TransactionInput)
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
  :: forall (address :: Type)
   . PlutusAddress address
  => address
  -> Contract UtxoMap
utxosAt addressAny = do
  networkId <- asks _.networkId
  let address = fromPlutusAddress networkId $ getAddress addressAny
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
  cardanoUtxoMap <- liftedE $ liftAff $ queryHandle.utxosAt address
  liftContractM "utxosAt: failed to convert utxos"
    $ toPlutusUtxoMap cardanoUtxoMap

utxosAtScriptHash
  :: ScriptHash -> Contract UtxoMap
utxosAtScriptHash hash = do
  queryHandle <- getQueryHandle
  cardanoUtxoMap <- liftedE $ liftAff $ queryHandle.utxosAtScriptHash hash
  liftContractM "utxosAtScriptHash: failed to convert utxos"
    $ toPlutusUtxoMap cardanoUtxoMap

-- | Queries for an utxo given a transaction input.
-- | Returns `Nothing` if the output has already been spent.
getUtxo
  :: TransactionInput
  -> Contract (Maybe TransactionOutput)
getUtxo oref = do
  queryHandle <- getQueryHandle
  cardanoTxOutput <- liftedE $ liftAff $ queryHandle.getUtxoByOref oref
  for cardanoTxOutput
    (liftContractM "getUtxo: failed to convert tx output" <<< toPlutusTxOutput)
