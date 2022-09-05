-- | A module for performing conversions between various types and
-- | their Plutus representations.
-- |
-- | Conversion functions come in pairs and must be named as follows:
-- | `fromPlutusType` and `toPlutusType`, where `Type` is to
-- | be replaced by the name of the actual type.
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

  -- Plutus UtxoMap <-> Cardano UtxoMap
  , fromPlutusUtxoMap
  , toPlutusUtxoMap
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse)

import Cardano.Types.Transaction (TransactionOutput, UtxoMap) as Cardano
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  ) as Cardano
import Cardano.Types.Value (Coin) as Cardano

import Plutus.Conversion.Address (fromPlutusAddress, toPlutusAddress)
import Plutus.Conversion.Value (fromPlutusValue, toPlutusValue)
import Plutus.Types.Transaction (TransactionOutput, UtxoMap) as Plutus
import Plutus.Types.TransactionUnspentOutput (TransactionUnspentOutput) as Plutus
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
  :: NetworkId -> Plutus.TransactionOutput -> Cardano.TransactionOutput
fromPlutusTxOutput networkId plutusTxOut =
  let
    rec = unwrap plutusTxOut
  in
    wrap
      { address: fromPlutusAddress networkId rec.address
      , amount: fromPlutusValue rec.amount
      , dataHash: rec.dataHash
      }

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
  -> Cardano.TransactionUnspentOutput
fromPlutusTxUnspentOutput networkId txUnspentOutput =
  let
    rec = unwrap txUnspentOutput
  in
    wrap
      { input: rec.input
      , output: fromPlutusTxOutput networkId rec.output
      }

toPlutusTxUnspentOutput
  :: Cardano.TransactionUnspentOutput
  -> Maybe Plutus.TransactionUnspentOutput
toPlutusTxUnspentOutput txUnspentOutput = do
  let rec = unwrap txUnspentOutput
  output <- toPlutusTxOutput rec.output
  pure $ wrap { input: rec.input, output }

--------------------------------------------------------------------------------
-- Plutus UtxoMap <-> Cardano UtxoMap
--------------------------------------------------------------------------------

fromPlutusUtxoMap :: NetworkId -> Plutus.UtxoMap -> Cardano.UtxoMap
fromPlutusUtxoMap networkId =
  map (fromPlutusTxOutput networkId)

toPlutusUtxoMap :: Cardano.UtxoMap -> Maybe Plutus.UtxoMap
toPlutusUtxoMap =
  traverse toPlutusTxOutput
