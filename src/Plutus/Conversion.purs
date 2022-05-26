module Plutus.Conversion
  (
    -- Plutus Address <-> CSL Address
    module Conversion.Address

  -- Plutus Value <-> Types.Value
  , module Conversion.Value

  -- Plutus Coin <-> Cardano Coin
  , fromPlutusCoin
  , toPlutusCoin

  -- Plutus TransactionOutput <-> Cardano TransactionOutput
  , fromPlutusTxOutput
  , toPlutusTxOutput

  -- Plutus TransactionUnspentOutput <-> Cardano TransactionUnspentOutput
  , fromPlutusTxUnspentOutput
  , toPlutusTxUnspentOutput

  -- Plutus UtxoM <-> Cardano UtxoM
  , fromPlutusUtxoM
  , toPlutusUtxoM
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse)

import Cardano.Types.Transaction (TransactionOutput, UtxoM) as Cardano
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  ) as Cardano
import Cardano.Types.Value (Coin) as Cardano

import Plutus.Conversion.Address (fromPlutusAddress, toPlutusAddress)
import Plutus.Conversion.Value (fromPlutusValue, toPlutusValue)
import Plutus.Types.Transaction (TransactionOutput, UtxoM(UtxoM)) as Plutus
import Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as Plutus
import Plutus.Types.Value (Coin) as Plutus

import Serialization.Address (NetworkId)

import Plutus.Conversion.Address
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  ) as Conversion.Address

import Plutus.Conversion.Value
  ( fromPlutusValue
  , toPlutusValue
  ) as Conversion.Value

--------------------------------------------------------------------------------
-- Plutus Coin <-> Cardano Coin
--------------------------------------------------------------------------------

fromPlutusCoin :: Plutus.Coin -> Cardano.Coin
fromPlutusCoin = wrap <<< unwrap

toPlutusCoin :: Cardano.Coin -> Plutus.Coin
toPlutusCoin = wrap <<< unwrap

--------------------------------------------------------------------------------
-- Plutus TransactionOutput <-> Cardano TransactionOutput
--------------------------------------------------------------------------------

fromPlutusTxOutput
  :: NetworkId -> Plutus.TransactionOutput -> Maybe Cardano.TransactionOutput
fromPlutusTxOutput networkId plutusTxOut = do
  let rec = unwrap plutusTxOut
  address <- fromPlutusAddress networkId rec.address
  let amount = fromPlutusValue rec.amount
  pure $ wrap { address, amount, dataHash: rec.dataHash }

toPlutusTxOutput
  :: Cardano.TransactionOutput -> Maybe Plutus.TransactionOutput
toPlutusTxOutput cardanoTxOut = do
  let rec = unwrap cardanoTxOut
  address <- toPlutusAddress rec.address
  let amount = toPlutusValue rec.amount
  pure $ wrap { address, amount, dataHash: rec.dataHash }

--------------------------------------------------------------------------------
-- Plutus TransactionUnspentOutput <-> Cardano TransactionUnspentOutput
--------------------------------------------------------------------------------

fromPlutusTxUnspentOutput
  :: NetworkId
  -> Plutus.TransactionUnspentOutput
  -> Maybe Cardano.TransactionUnspentOutput
fromPlutusTxUnspentOutput networkId txUnspentOutput = do
  let rec = unwrap txUnspentOutput
  output <- fromPlutusTxOutput networkId rec.output
  pure $ wrap { input: rec.input, output }

toPlutusTxUnspentOutput
  :: Cardano.TransactionUnspentOutput
  -> Maybe Plutus.TransactionUnspentOutput
toPlutusTxUnspentOutput txUnspentOutput = do
  let rec = unwrap txUnspentOutput
  output <- toPlutusTxOutput rec.output
  pure $ wrap { input: rec.input, output }

--------------------------------------------------------------------------------
-- Plutus UtxoM <-> Cardano UtxoM
--------------------------------------------------------------------------------

fromPlutusUtxoM :: NetworkId -> Plutus.UtxoM -> Maybe Cardano.UtxoM
fromPlutusUtxoM networkId =
  map wrap <<< traverse (fromPlutusTxOutput networkId) <<< unwrap

toPlutusUtxoM :: Cardano.UtxoM -> Maybe Plutus.UtxoM
toPlutusUtxoM =
  map wrap <<< traverse toPlutusTxOutput <<< unwrap
