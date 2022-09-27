-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( getUtxo
  , getWalletBalance
  , getWalletUtxos
  , utxosAt
  , module X
  ) where

import Prelude

import Contract.Monad (Contract, liftContractM, wrapContract)
import Contract.Prelude (for)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Plutus.Conversion (fromPlutusAddress, toPlutusTxOutput, toPlutusUtxoMap)
import Plutus.Conversion.Value (toPlutusValue)
import Plutus.Types.Address (class PlutusAddress, getAddress)
import Plutus.Types.Transaction (UtxoMap)
import Plutus.Types.Transaction (UtxoMap) as X
import Plutus.Types.Value (Value)
import QueryM.Utxos (getUtxo, getWalletBalance, getWalletUtxos, utxosAt) as Utxos

-- | This module defines query functionality via Ogmios to get utxos.

-- | Gets utxos at an (internal) `Address` in terms of a Plutus `Address`.
-- | Results may vary depending on `Wallet` type. See `QueryM` for more details
-- | on wallet variance.
-- |
-- | NOTE: Querying for UTxOs by address is deprecated. See
-- | [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues/536).
utxosAt
  :: forall (r :: Row Type) (a :: Type)
   . PlutusAddress a
  => a
  -> Contract r (Maybe UtxoMap)
utxosAt address = do
  networkId <- asks (_.networkId <<< _.config <<< unwrap)
  let cardanoAddr = fromPlutusAddress networkId (getAddress address)
  -- Don't error if we get `Nothing` as the Cardano utxos
  mCardanoUtxos <- wrapContract $ Utxos.utxosAt cardanoAddr
  for mCardanoUtxos
    (liftContractM "utxosAt: unable to deserialize utxos" <<< toPlutusUtxoMap)

-- | Queries for UTxO given a transaction input.
getUtxo
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract r (Maybe TransactionOutput)
getUtxo ref = do
  cardanoTxOut <- wrapContract $ Utxos.getUtxo ref
  for cardanoTxOut
    (liftContractM "getUtxo: unable to deserialize UTxO" <<< toPlutusTxOutput)

getWalletBalance
  :: forall (r :: Row Type)
   . Contract r (Maybe Value)
getWalletBalance = wrapContract (Utxos.getWalletBalance <#> map toPlutusValue)

-- | Similar to `utxosAt` called on own address, except that it uses CIP-30
-- | wallet state and not query layer state.
-- | The user should not expect these states to be in sync.
-- | When active wallet is `KeyWallet`, query layer state is used.
-- | This function is expected to be more performant than `utxosAt` when there
-- | is a large number of assets.
getWalletUtxos
  :: forall (r :: Row Type)
   . Contract r (Maybe UtxoMap)
getWalletUtxos = do
  mCardanoUtxos <- wrapContract Utxos.getWalletUtxos
  for mCardanoUtxos $
    liftContractM "getWalletUtxos: unable to deserialize UTxOs" <<<
      toPlutusUtxoMap
