-- | A module that defines the different Utxo `Data.Map`s from transaction
-- | input to transaction output. Furthermore, a helper to get the utxos at
-- | a given `Address` is defined.
module Contract.Utxos
  ( getUtxo
  , getWalletBalance
  , module Transaction
  , utxosAt
  ) where

import Prelude

import Contract.Monad (Contract, liftContractM, wrapContract)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Plutus.Conversion (fromPlutusAddress, toPlutusTxOutput, toPlutusUtxoM)
import Plutus.Conversion.Value (toPlutusValue)
import Plutus.Types.Address (Address)
import Plutus.Types.Transaction (UtxoM(UtxoM)) as Transaction
import Plutus.Types.Value (Value)
import QueryM.Utxos (getUtxo, getWalletBalance, utxosAt) as Utxos

-- | This module defines query functionality via Ogmios to get utxos.

-- | Gets utxos at an (internal) `Address` in terms of a Plutus `Address`.
-- | Results may vary depending on `Wallet` type. See `QueryM` for more details
-- | on wallet variance.
utxosAt
  :: forall (r :: Row Type). Address -> Contract r (Maybe Transaction.UtxoM)
utxosAt address = do
  networkId <- asks (_.networkId <<< unwrap)
  let cardanoAddr = fromPlutusAddress networkId address
  -- Don't error if we get `Nothing` as the Cardano utxos
  mCardanoUtxos <- wrapContract $ Utxos.utxosAt cardanoAddr
  maybe (pure Nothing)
    ( map Just <<< liftContractM "utxosAt: unable to deserialize utxos" <<<
        toPlutusUtxoM
    )
    mCardanoUtxos

-- | `getUtxo ref` queries for the utxo given by `ref`.
getUtxo
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract r (Maybe TransactionOutput)
getUtxo ref = do
  cardanoTxOut <- wrapContract $ Utxos.getUtxo ref
  maybe (pure Nothing)
    ( map Just <<< liftContractM "getUtxo: unable to deserialize utxo" <<<
        toPlutusTxOutput
    )
    cardanoTxOut

getWalletBalance
  :: forall (r :: Row Type)
   . Contract r (Maybe Value)
getWalletBalance = wrapContract (Utxos.getWalletBalance <#> map toPlutusValue)
