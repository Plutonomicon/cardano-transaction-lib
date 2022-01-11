module Value
  ( flattenValue
  , isAdaOnly
  ) where

import Prelude

import Control.Alternative (guard)
import Data.BigInt (BigInt)
import Data.List ((:), List(..))
import Data.Map (Map, toUnfoldable)
import Data.Tuple.Nested ((/\), type (/\))

import Types.Transaction (CurrencySymbol(..), TokenName(..), Value(..))

-- This module rewrites functionality from:
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value

-- Could use Data.Newtype (unwrap) too.
getValue
  :: Value
  -> Map CurrencySymbol (Map TokenName BigInt)
getValue (Value v) = v

-- Taken from https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
flattenValue
  :: Value
  -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue v = do
    cs /\ m <- toUnfoldable <<< getValue $ v
    tn /\ a <- toUnfoldable m
    guard $ a /= zero
    pure $ cs /\ tn /\ a

-- | Predicate on whether some Value contains Ada only.
isAdaOnly :: Value -> Boolean
isAdaOnly v =
  case flattenValue v of
    (CurrencySymbol "" /\ TokenName "" /\ _) : Nil -> true
    _ -> false