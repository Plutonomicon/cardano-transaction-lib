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

  -- Plutus TransactionOutputWithRefScript <-> Cardano TransactionOutput
  , fromPlutusTxOutputWithRefScript
  , toPlutusTxOutputWithRefScript

  -- Plutus TransactionUnspentOutput <-> Cardano TransactionUnspentOutput
  , fromPlutusTxUnspentOutput
  , toPlutusTxUnspentOutput

  -- Plutus UtxoM <-> Cardano UtxoM
  , fromPlutusUtxoM
  , toPlutusUtxoM
  ) where

import Prelude

import Cardano.Types.ScriptRef (ScriptRef, scriptRefHash)
import Cardano.Types.Transaction (TransactionOutput, UtxoM) as Cardano
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput) as Cardano
import Cardano.Types.Value (Coin) as Cardano
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse)
import Plutus.Conversion.Address
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  ) as Conversion.Address
import Plutus.Conversion.Address (fromPlutusAddress, toPlutusAddress)
import Plutus.Conversion.Value (fromPlutusValue, toPlutusValue)
import Plutus.Conversion.Value (fromPlutusValue, toPlutusValue) as Conversion.Value
import Plutus.Types.Transaction
  ( TransactionOutput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , UtxoM
  ) as Plutus
import Plutus.Types.TransactionUnspentOutput (TransactionUnspentOutput) as Plutus
import Plutus.Types.Value (Coin) as Plutus
import Serialization.Address (NetworkId)

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
  :: NetworkId
  -> Maybe ScriptRef
  -> Plutus.TransactionOutput
  -> Cardano.TransactionOutput
fromPlutusTxOutput networkId scriptRef plutusTxOut =
  let
    rec = unwrap plutusTxOut
  in
    wrap
      { address: fromPlutusAddress networkId rec.address
      , amount: fromPlutusValue rec.amount
      , datum: rec.datum
      , scriptRef
      }

toPlutusTxOutput
  :: Cardano.TransactionOutput -> Maybe Plutus.TransactionOutput
toPlutusTxOutput cardanoTxOut = do
  let rec = unwrap cardanoTxOut
  address <- toPlutusAddress rec.address
  let
    amount = toPlutusValue rec.amount
    referenceScript = scriptRefHash =<< rec.scriptRef
  pure $ wrap
    { address, amount, datum: rec.datum, referenceScript }

--------------------------------------------------------------------------------
-- Plutus TransactionOutputWithRefScript <-> Cardano TransactionOutput
--------------------------------------------------------------------------------

fromPlutusTxOutputWithRefScript
  :: NetworkId
  -> Plutus.TransactionOutputWithRefScript
  -> Cardano.TransactionOutput
fromPlutusTxOutputWithRefScript
  networkId
  (Plutus.TransactionOutputWithRefScript { output, scriptRef }) =
  fromPlutusTxOutput networkId scriptRef output

toPlutusTxOutputWithRefScript
  :: Cardano.TransactionOutput -> Maybe Plutus.TransactionOutputWithRefScript
toPlutusTxOutputWithRefScript cTxOutput =
  toPlutusTxOutput cTxOutput
    <#> wrap <<< { output: _, scriptRef: (unwrap cTxOutput).scriptRef }

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
      , output: fromPlutusTxOutputWithRefScript networkId rec.output
      }

toPlutusTxUnspentOutput
  :: Cardano.TransactionUnspentOutput
  -> Maybe Plutus.TransactionUnspentOutput
toPlutusTxUnspentOutput txUnspentOutput = do
  let rec = unwrap txUnspentOutput
  output <- toPlutusTxOutputWithRefScript rec.output
  pure $ wrap { input: rec.input, output }

--------------------------------------------------------------------------------
-- Plutus UtxoM <-> Cardano UtxoM
--------------------------------------------------------------------------------

fromPlutusUtxoM :: NetworkId -> Plutus.UtxoM -> Cardano.UtxoM
fromPlutusUtxoM networkId =
  wrap <<< map (fromPlutusTxOutputWithRefScript networkId) <<< unwrap

toPlutusUtxoM :: Cardano.UtxoM -> Maybe Plutus.UtxoM
toPlutusUtxoM =
  map wrap <<< traverse toPlutusTxOutputWithRefScript <<< unwrap
