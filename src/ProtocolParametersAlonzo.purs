module ProtocolParametersAlonzo
  ( coinSize
  , lovelacePerUTxOWord
  , pidSize
  , protocolParamUTxOCostPerWord
  , utxoEntrySizeWithoutVal
  -- , Word(..)
  ) where

import Prelude
import Data.BigInt (BigInt, fromInt)
-- import Data.Newtype (class Newtype)

import Types.Ada (Ada(..))

-- newtype Word = Word BigInt
-- derive instance eqWord :: Eq Word
-- derive instance newtypeWord :: Newtype Word _
-- derive instance ordWord :: Ord Word
-- derive newtype instance showWord :: Show Word

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-pab/html/src/Cardano.Api.ProtocolParameters.html
-- Shelley params, is this unchanged?
protocolParamUTxOCostPerWord :: Ada
protocolParamUTxOCostPerWord = Lovelace $ fromInt 1

-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028
lovelacePerUTxOWord :: Ada
lovelacePerUTxOWord = Lovelace $ fromInt 34482

-- words
-- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
utxoEntrySizeWithoutVal :: BigInt
utxoEntrySizeWithoutVal = fromInt 27

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
pidSize :: BigInt
pidSize = fromInt 28

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
coinSize :: BigInt
coinSize = fromInt 2