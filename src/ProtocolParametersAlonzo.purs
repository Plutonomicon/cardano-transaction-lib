module ProtocolParametersAlonzo
  ( lovelacePerUTxOWord
  , protocolParamUTxOCostPerWord
  ) where

import Prelude
import Data.BigInt (fromInt)
import Data.Maybe (Maybe(..))

import Types.Transaction (Ada(..))

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-pab/html/src/Cardano.Api.ProtocolParameters.html
-- Shelley params, is this unchanged?
protocolParamUTxOCostPerWord :: Ada
protocolParamUTxOCostPerWord = Lovelace <<< fromInt $ 1

-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028
lovelacePerUTxOWord :: Ada
lovelacePerUTxOWord = Lovelace <<< fromInt $ 34482