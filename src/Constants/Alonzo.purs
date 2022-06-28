module Constants.Alonzo
  ( adaOnlyWords
  , coinSize
  , pidSize
  , utxoEntrySizeWithoutVal
  ) where

import Prelude

import Cardano.Types.Value (Coin(Coin))
import Data.BigInt (BigInt, fromInt)

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

-- An ada-only UTxO entry is 29 words. More details about min utxo
-- calculation can be found here:
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
adaOnlyWords :: BigInt
adaOnlyWords = fromInt 29
