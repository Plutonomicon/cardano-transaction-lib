module Ada
  ( adaSymbol
  , adaToken
  , lovelaceValueOf
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Map (singleton)

import Types.Transaction (CurrencySymbol(..), TokenName(..), Value(..))

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol ""

adaToken :: TokenName
adaToken = TokenName ""

lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = Value <<< singleton adaSymbol <<< singleton adaToken