module ProtocolParametersAlonzo
  ( adaOnlyWords
  , coinSize
  , costModels
  , lovelacePerUTxOWord
  , minAdaTxOut
  , pidSize
  , protocolParamUTxOCostPerWord
  , utxoEntrySizeWithoutVal
  ) where

import Prelude

import Cardano.Types.Transaction
  ( Costmdls(Costmdls)
  , CostModel(CostModel)
  , Language(PlutusV1)
  )
import Cardano.Types.Value (Coin(Coin))
import Data.BigInt (BigInt, fromInt)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-pab/html/src/Cardano.Api.ProtocolParameters.html
-- Shelley params, is this unchanged?
protocolParamUTxOCostPerWord :: Coin
protocolParamUTxOCostPerWord = Coin $ fromInt 1

-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028
lovelacePerUTxOWord :: Coin
lovelacePerUTxOWord = Coin $ fromInt 34482

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

-- Minimum required Ada for each tx output.
minAdaTxOut :: Coin
minAdaTxOut = Coin $ fromInt 2_000_000

-- An ada-only UTxO entry is 29 words. More details about min utxo
-- calculation can be found here:
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
adaOnlyWords :: BigInt
adaOnlyWords = fromInt 29

costModels :: Costmdls
costModels = Costmdls $ Map.fromFoldable
  -- Note that the ordering of the cost models does not match the ordering of
  -- of the keys in the JSON-encoded protocol params
  --
  -- See https://github.com/Emurgo/cardano-serialization-lib/issues/281 for
  -- more details on the ordering below
  [ PlutusV1 /\
      ( CostModel $
          UInt.fromInt <$>
            [ 197209
            , 0
            , 1
            , 1
            , 396231
            , 621
            , 0
            , 1
            , 150000
            , 1000
            , 0
            , 1
            , 150000
            , 32
            , 2477736
            , 29175
            , 4
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 100
            , 100
            , 29773
            , 100
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 1000
            , 0
            , 1
            , 150000
            , 32
            , 150000
            , 1000
            , 0
            , 8
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 150000
            , 1000
            , 0
            , 8
            , 150000
            , 112536
            , 247
            , 1
            , 150000
            , 10000
            , 1
            , 136542
            , 1326
            , 1
            , 1000
            , 150000
            , 1000
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 1
            , 1
            , 150000
            , 1
            , 150000
            , 4
            , 103599
            , 248
            , 1
            , 103599
            , 248
            , 1
            , 145276
            , 1366
            , 1
            , 179690
            , 497
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 61516
            , 11218
            , 0
            , 1
            , 150000
            , 32
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 2477736
            , 29175
            , 4
            , 0
            , 82363
            , 4
            , 150000
            , 5000
            , 0
            , 1
            , 150000
            , 32
            , 197209
            , 0
            , 1
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 3345831
            , 1
            , 1
            ]
      )
  ]
